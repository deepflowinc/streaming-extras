{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Network.S3.Sign.V4 (
  S3Config (..),
  S3Keys (..),
  S3Region (..),
  Utf8 (..),
  S3Endpoint (..),
  getPort,
  signRequest,
  RequestSignSeed (..),
) where

import Control.Lens hiding ((:>))
import Control.Monad (unless)
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), withText)
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.CaseInsensitive as CI
import Data.Coerce (coerce)
import Data.Digest.Pure.SHA.Streaming
import Data.Function (on)
import Data.Functor.Of (Of (..))
import Data.Generics.Labels ()
import qualified Data.HashSet as HS
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable (intercalate1, intercalateMap1)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Simple (setRequestBodyLBS, setRequestHeaders)
import Network.HTTP.Streaming (fromRequestBody)
import Network.HTTP.Types
import Network.URI (URI (uriScheme), URIAuth (..), parseURI, uriAuthority, uriIsAbsolute, uriPort)
import qualified Streaming.ByteString as Q
import Text.Read (readMaybe)

newtype Utf8 = Utf8 {getUtf8 :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSONKey, ToJSONKey)

instance IsString Utf8 where
  fromString = Utf8 . T.encodeUtf8 . T.pack
  {-# INLINE fromString #-}

instance FromJSON Utf8 where
  parseJSON = withText "UTF-8 encoded string" $ pure . Utf8 . T.encodeUtf8
  {-# INLINE parseJSON #-}

instance ToJSON Utf8 where
  toJSON = toJSON . T.decodeUtf8 . view #getUtf8
  {-# INLINE toJSON #-}

data S3Keys = S3Keys
  { accessKeySecret :: Utf8
  , accessKeyId :: Utf8
  }
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

newtype S3Region = S3Region {runS3Region :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Utf8

newtype S3Endpoint = S3Endpoint {getS3Endpoint :: URI}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show)

instance ToJSON S3Endpoint where
  toJSON = toJSON . show

instance FromJSON S3Endpoint where
  parseJSON = withText "uri" $ \url -> do
    uri <- maybe (fail $ "Invalid URI: " <> show url) pure $ parseURI $ T.unpack url
    unless (uriIsAbsolute uri) $
      fail $
        "Not absoltue: " <> show uri
    unless (uriScheme uri `HS.member` HS.fromList ["http:", "https:"]) $
      fail $
        "URI with invalid scheme (http or https required): " <> show uri
    pure $ S3Endpoint uri

data S3Config = S3Config
  { keys :: S3Keys
  , s3Endpoint :: S3Endpoint
  , region :: S3Region
  }
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

getPort :: S3Endpoint -> Int
getPort ep@(S3Endpoint uri) =
  fromMaybe
    (if isSecure ep then 443 else 80)
    $ readMaybe . drop 1 . uriPort =<< uriAuthority uri

isSecure :: S3Endpoint -> Bool
isSecure = (== "https:") . uriScheme . coerce

data SignEnv = SignEnv {s3Config :: S3Config, timeStamp :: UTCTime}
  deriving (Eq, Ord, Generic)

signRequest :: S3Config -> Request -> IO Request
signRequest s3Config@S3Config {..} req0 = do
  timeStamp <- getCurrentTime
  let hdr' =
        Map.insert "Host" (BS8.pack $ toHostHeader s3Endpoint) $
          Map.insert "Date" (BS8.pack $ formatAWSDateTime timeStamp) $
            Map.insert "x-amz-date" (BS8.pack $ formatAWSDateTime timeStamp) $
              Map.delete hAuthorization $
                Map.fromList $
                  requestHeaders req0
      reqToSign =
        req0
          { requestHeaders = Map.toList hdr'
          , host = BS8.pack $ toHostHeader s3Endpoint
          , port = getPort s3Endpoint
          , secure = isSecure s3Endpoint
          }
  toCanonicalRequest reqToSign
    & insertAuthorizationHeader SignEnv {..}
    & toRequest reqToSign

toRequest :: Request -> Q.ByteStream IO RequestHeaders -> IO Request
toRequest req0 bs = do
  -- FIXME: can't we do this more efficiently?
  lbs :> hdrs <- Q.toLazy bs
  pure $ setRequestHeaders hdrs $ setRequestBodyLBS lbs req0

toHostHeader :: S3Endpoint -> String
toHostHeader (S3Endpoint uri) =
  maybe (error "No Authority") uriRegName $ uriAuthority uri

signingAlgorithm :: BS8.ByteString
signingAlgorithm = "AWS4-HMAC-SHA256"

insertAuthorizationHeader ::
  (Monad m) =>
  SignEnv ->
  Q.ByteStream m RequestSignSeed ->
  Q.ByteStream m RequestHeaders
insertAuthorizationHeader senv = fmap $ \RequestSignSeed {..} ->
  let sgn = runReader (toAuthorizationHeader canonicalRequest) senv
   in Map.toList $ Map.insert hAuthorization sgn $ Map.fromList headersWithHash

toAuthorizationHeader :: (Monad m) => CanonicalRequest -> ReaderT SignEnv m BS8.ByteString
toAuthorizationHeader canon = do
  keyId <- view $ #s3Config . #keys . #accessKeyId . #_Utf8
  str2Sgn <- stringToSign canon
  signature <- generateSign str2Sgn
  credScope <- credentialScope
  pure $
    LBS.toStrict $
      BB.toLazyByteString $
        BB.byteString signingAlgorithm
          <> " "
          <> intercalateMap1
            @NonEmpty
            ","
            (\(l, r) -> l <> "=" <> r)
            [ ("Credential", BB.byteString keyId <> "/" <> credScope)
            , ("SignedHeaders", canon ^. #signedHeaders)
            , ("Signature", BB.byteString signature)
            ]

credentialScope :: (Monad m) => ReaderT SignEnv m BB.Builder
credentialScope = do
  timeStamp <- view #timeStamp
  S3Region region <- view $ #s3Config . #region
  pure $ fromString (formatAWSDate timeStamp) <> "/" <> BB.byteString region <> "/s3/aws4_request"

formatAWSDateTime :: UTCTime -> String
formatAWSDateTime = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

formatAWSDate :: UTCTime -> String
formatAWSDate = formatTime defaultTimeLocale "%Y%m%d"

newtype StringToSign = StringToSign {runStringToSign :: LBS.ByteString}
  deriving (Show, Eq, Ord, Generic)

generateSign :: (Monad m) => StringToSign -> ReaderT SignEnv m BS.ByteString
generateSign (StringToSign strToSgn) = do
  now <- view #timeStamp
  secret <- view $ #s3Config . #keys . #accessKeySecret . #_Utf8
  region <- view $ #s3Config . #region . #_S3Region
  let kDate =
        hmacSha256BS ("AWS4" <> LBS.fromStrict secret) $
          LBS8.pack $
            formatAWSDate now
      kRegion = hmacSha256BS kDate $ LBS.fromStrict region
      kService = hmacSha256BS kRegion "s3"
      kSigning = hmacSha256BS kService "aws4_request"
  pure $ BS8.pack $ showDigest $ hmacSha256 kSigning strToSgn

hmacSha256BS :: LBS8.ByteString -> LBS8.ByteString -> LBS8.ByteString
hmacSha256BS = fmap bytestringDigest . hmacSha256

stringToSign :: (Monad m) => CanonicalRequest -> ReaderT SignEnv m StringToSign
stringToSign canonReq = do
  timeStamp <- view #timeStamp
  credScope <- credentialScope
  pure $
    StringToSign $
      BB.toLazyByteString $
        intercalate1 @NonEmpty
          "\n"
          [ BB.byteString signingAlgorithm
          , fromString $ formatAWSDateTime timeStamp
          , credScope
          , fromString $
              showDigest $
                sha256 $
                  BB.toLazyByteString $
                    formatCanonicalRequest canonReq
          ]

data CanonicalRequest = CanonicalRequest
  { httpMethod :: BB.Builder
  , canonicalUri :: BB.Builder
  , canonicalQueryString :: BB.Builder
  , canonicalHeaders :: BB.Builder
  , signedHeaders :: BB.Builder
  , hashedPayload :: BB.Builder
  }
  deriving (Show, Generic)

data RequestSignSeed = RequestSignSeed
  { canonicalRequest :: CanonicalRequest
  , headersWithHash :: RequestHeaders
  }
  deriving (Show, Generic)

toCanonicalRequest ::
  Request ->
  Q.ByteStream IO RequestSignSeed
toCanonicalRequest req = do
  hashedPayload <-
    fromString . showDigest
      <$> sha256Stream_ (Q.copy $ fromRequestBody $ requestBody req)
  let httpMethod = BB.byteString $ method req
      canonicalUri = BB.byteString $ path req
      headersWithHash =
        Map.toList $
          Map.insert "x-amz-content-sha256" (LBS.toStrict $ BB.toLazyByteString hashedPayload) $
            Map.fromList $
              requestHeaders req
      canonicalQueryString =
        maybe
          mempty
          ( intercalateMap1 "&" $ \(key, val) ->
              BB.byteString key <> "=" <> val
          )
          $ NE.nonEmpty
          $ sortOn fst
          $ map
            ( bimap
                (urlEncode True)
                (maybe mempty (urlEncodeBuilder True))
            )
          $ parseQuery
          $ queryString req
      hdrs =
        fmap (\kvs@((k, _) :| _) -> (k, fmap snd kvs)) $
          NE.groupBy1 ((==) `on` fst) $
            NE.fromList $
              sortOn fst $
                map (Bi.first CI.foldedCase) $
                  filter
                    (isTargetHeader . fst)
                    headersWithHash
      signedHeaders =
        intercalateMap1 ";" (BB.byteString . fst) hdrs
      canonicalHeaders =
        intercalateMap1
          "\n"
          (\(hdr, vals) -> BB.byteString hdr <> ":" <> canonicaliseHeaderVal vals)
          hdrs
      canonicalRequest = CanonicalRequest {..}
  pure RequestSignSeed {..}

isTargetHeader :: HeaderName -> Bool
isTargetHeader hdr =
  hdr `HS.member` ["Content-Type", "Host", ":authority"]
    || (CI.foldCase "x-amz-" `BS.isPrefixOf` CI.foldedCase hdr)

canonicaliseHeaderVal :: NonEmpty BS8.ByteString -> BB.Builder
canonicaliseHeaderVal =
  intercalateMap1 "," $
    BB.byteString . BS8.unwords . BS8.words . BS8.strip

formatCanonicalRequest :: CanonicalRequest -> BB.Builder
formatCanonicalRequest CanonicalRequest {..} =
  intercalate1 @NonEmpty
    "\n"
    [ httpMethod
    , canonicalUri
    , canonicalQueryString
    , canonicalHeaders
    , mempty
    , signedHeaders
    , hashedPayload
    ]
