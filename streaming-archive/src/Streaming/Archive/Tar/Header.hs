{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

-- Implements ustar
module Streaming.Archive.Tar.Header (
  Header (..),
  TypeFlag (..),
  TarException (..),
  ErrorContext,
  hdrFullName,
  hdrUserName,
  hdrGroupName,
  hdrModifiedTime,
  dataBlockCount,

  -- ** Checksum
  checksum,

  -- ** Formatting
  formatHeader,

  -- ** Parsing
  parseHeader,
  readDataBlock,

  -- *** Aux utilities
  headerP,
  typeFlagP,
  anyBytes,
  takeOctalNumeral,
  takeShortByteString,
) where

import Control.Applicative ((<|>))
import Control.Comonad (duplicate)
import Control.Exception (Exception)
import Control.Foldl qualified as L
import Control.Lens (view)
import Control.Monad ((<$!>))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Attoparsec.ByteString.Streaming qualified as AQ
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.Extra qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Function ((&))
import Data.Functor (void)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Of (Of (..))
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.List qualified as List
import Data.Monoid (Dual (..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word
import Foreign.C (CTime (..))
import GHC.Generics (Generic)
import Streaming.ByteString qualified as Q
import System.Posix.Types (EpochTime, FileMode, GroupID, UserID)

data Header = Header
  { name :: {-# UNPACK #-} !ShortByteString
  , mode :: !FileMode
  , uid :: !UserID
  , gid :: !GroupID
  , size :: !Word64
  , mtime :: !EpochTime
  , typeflag :: !TypeFlag
  , linkName :: {-# UNPACK #-} !ShortByteString
  , magic :: {-# UNPACK #-} !ShortByteString
  , version :: {-# UNPACK #-} !ShortByteString
  , uname :: {-# UNPACK #-} !ShortByteString
  , gname :: {-# UNPACK #-} !ShortByteString
  , deviceMajor :: {-# UNPACK #-} !ShortByteString
  , deviceMinor :: {-# UNPACK #-} !ShortByteString
  , namePrefix :: {-# UNPACK #-} !ShortByteString
  }
  deriving (Show, Eq, Ord, Generic)

type ErrorContext = [String]

data TarException
  = HeaderFormatError String ErrorContext
  | InvalidHeaderChecksum !Word32 !Word32 !Header
  | NotEnoughBytes ShortByteString !Word64 !Word64
  | InvalidEndOfArchive
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

dataBlockCount :: Header -> Int64
dataBlockCount Header {..} =
  case typeflag of
    Symlink -> 0
    DevLink -> 0
    Dir -> 0
    FIFOSpecial -> 0
    _ -> fromIntegral $ (size + 511) `quot` 512

readDataBlock ::
  (Monad m) =>
  Header ->
  Q.ByteStream m a ->
  m (Either TarException LBS.ByteString, Q.ByteStream m a)
readDataBlock hdr@Header {..} bs = do
  Q.splitAt (512 * dataBlockCount hdr) bs
    & Q.splitAt (fromIntegral size)
    & Q.copy
    & Q.length
    & Q.toLazy
    >>= \(chnk :> len :> rest) ->
      if len < fromIntegral size
        then (Left $ NotEnoughBytes (hdrFullName hdr) size (fromIntegral len),) <$> Q.effects rest
        else (Right chnk,) <$> Q.effects rest

parseHeader :: (Monad m) => Q.ByteStream m a -> m (Either TarException Header, Q.ByteStream m a)
parseHeader q = do
  (eith, rest) <-
    q
      & Q.splitAt 512
      & Q.copy
      & checksum
      & AQ.parse headerP

  case eith of
    Left (ctx, err) -> do
      _ :> bs <- Q.effects rest
      pure (Left (HeaderFormatError err ctx), bs)
    Right RawHeader {..} -> do
      csum :> bs <- Q.effects rest
      if csum == chksum
        then pure (Right header, bs)
        else pure (Left $ InvalidHeaderChecksum csum chksum header, bs)

data RawHeader = RawHeader
  { header :: {-# UNPACK #-} !Header
  , chksum :: !Word32
  }
  deriving (Show, Eq, Ord)

checksum :: (Monad m) => Q.ByteStream m a -> m (Of Word32 a)
checksum bytes = do
  lsum :> lo <-
    bytes
      & Q.splitAt 512
      & Q.splitAt 148
      & L.purely Q.fold (duplicate $ L.premap fromIntegral L.sum)
  rsum :> ans' <-
    lo & Q.drop 8 & L.purely Q.fold lsum
  resl <- Q.effects ans'
  pure $! rsum + 256 :> resl

data TypeFlag = Regular | Symlink | DevLink | CharSpecial | BlockSpecial | Dir | FIFOSpecial | OtherType Char
  deriving (Show, Eq, Ord, Generic)

typeFlagP :: Atto.Parser TypeFlag
typeFlagP =
  Regular <$ (Atto.char '\NUL' <|> Atto.char '0')
    <|> Symlink <$ Atto.char '1'
    <|> DevLink <$ Atto.char '2'
    <|> CharSpecial <$ Atto.char '3'
    <|> BlockSpecial <$ Atto.char '4'
    <|> Dir <$ Atto.char '5'
    <|> FIFOSpecial <$ Atto.char '6'
    <|> OtherType <$> Atto.anyChar

anyBytes ::
  (Bits a, Num a) =>
  Int ->
  Atto.Parser a
anyBytes len =
  BS.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0 <$!> Atto.take len

{- |
Parses ASCII octal numeral of a given length.

>>> 0o644
420

>>> Atto.parseOnly  (takeOctalNumeral 3) "644"
Right 420

>>> 0o00015327
6871

>>> Atto.parseOnly  (takeOctalNumeral 8) "00015327"
Right 6871
-}
takeOctalNumeral :: (Bits a, Num a) => Int -> Atto.Parser a
takeOctalNumeral len =
  BS.foldl' (\ !t !c -> t `shiftL` 3 .|. fromIntegral (c - zero)) 0
    . BS.takeWhile (/= 0)
    <$> Atto.take len
  where
    !zero = 0x30 -- '0'

takeShortByteString :: Int -> Atto.Parser ShortByteString
takeShortByteString len = SBS.toShort . BS.takeWhile (/= 0) <$> Atto.take len

headerP :: Atto.Parser RawHeader
headerP = do
  name <- takeShortByteString 100
  mode <- takeOctalNumeral 8
  uid <- takeOctalNumeral 8
  gid <- takeOctalNumeral 8
  size <- takeOctalNumeral 12
  mtime <- CTime <$> takeOctalNumeral 12
  chksum <- takeOctalNumeral 7 <* Atto.anyChar
  typeflag <- typeFlagP
  linkName <- takeShortByteString 100
  magic <- takeShortByteString 6
  version <- takeShortByteString 2
  uname <- takeShortByteString 32
  gname <- takeShortByteString 32
  deviceMajor <- takeShortByteString 8
  deviceMinor <- takeShortByteString 8
  namePrefix <- takeShortByteString 155
  void $ Atto.count 12 Atto.anyChar
  pure RawHeader {header = Header {..}, ..}

formatSBS :: Int -> ShortByteString -> BB.Builder
formatSBS expLen sbs =
  BB.shortByteString sbs
    <> BB.byteString (BS.replicate (max 0 $ expLen - SBS.length sbs) 0)

calcChecksum :: Header -> Word32
calcChecksum =
  (\(a :> _) -> a)
    . runIdentity
    . checksum
    . Q.fromLazy
    . BB.toLazyByteStringWith (BB.untrimmedStrategy 512 512) ""
    . formatHeaderWithChecksum Nothing

formatHeader :: Header -> BB.Builder
formatHeader header =
  formatHeaderWithChecksum (Just $ calcChecksum header) header

formatHeaderWithChecksum :: Maybe Word32 -> Header -> BB.Builder
formatHeaderWithChecksum mchk Header {..} =
  mconcat
    [ formatSBS 100 name
    , formatOctNumNUL 8 mode
    , formatOctNumNUL 8 uid
    , formatOctNumNUL 8 gid
    , formatOctNumNUL 12 size
    , formatOctNumNUL 12 (mtime & \(CTime a) -> a)
    , maybe "        " (formatOctNumNUL 7 <> const " ") mchk
    , formatTypeFlag typeflag
    , formatSBS 100 linkName
    , formatSBS 6 magic
    , formatSBS 2 version
    , formatSBS 32 uname
    , formatSBS 32 gname
    , formatSBS 8 deviceMajor
    , formatSBS 8 deviceMinor
    , formatSBS 155 namePrefix
    , BB.shortByteString $ SBS.replicate 12 0
    ]

formatTypeFlag :: TypeFlag -> BB.Builder
formatTypeFlag Regular = BB.char8 '0'
formatTypeFlag Symlink = BB.char8 '1'
formatTypeFlag DevLink = BB.char8 '2'
formatTypeFlag CharSpecial = BB.char8 '3'
formatTypeFlag BlockSpecial = BB.char8 '4'
formatTypeFlag Dir = BB.char8 '5'
formatTypeFlag FIFOSpecial = BB.char8 '6'
formatTypeFlag (OtherType c) = BB.char8 c

{- |
@formatOctNumNUL n bits@ formats @bits@ as an $(n - 1)$-digit octal numeral with terminating @NUL@.

WARNING: It ignores bits 8^(n - 1) - Use with care!
-}
formatOctNumNUL :: (Bits a, Integral a) => Int -> a -> BB.Builder
formatOctNumNUL n bits =
  getDual
    ( foldMap (Dual . BB.word8) $
        take (n - 1) $
          List.unfoldr (\ !k -> Just (fromIntegral $ 0x30 + (k .&. 7), k `shiftR` 3)) bits
    )
    <> BB.word8 0

hdrFullName :: Header -> ShortByteString
hdrFullName Header {..} =
  if magic == "bsdtar " && not (SBS.null namePrefix)
    then namePrefix <> name
    else name

hdrUserName :: Header -> Maybe ShortByteString
hdrUserName Header {..} =
  if SBS.null uname
    then Nothing
    else Just uname

hdrGroupName :: Header -> Maybe ShortByteString
hdrGroupName Header {..} =
  if SBS.null gname
    then Nothing
    else Just gname

hdrModifiedTime :: Header -> UTCTime
hdrModifiedTime = posixSecondsToUTCTime . realToFrac . view #mtime
