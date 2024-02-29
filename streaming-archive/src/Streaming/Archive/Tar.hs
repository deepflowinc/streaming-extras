{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields -funbox-strict-fields #-}

-- GNU Tar
module Streaming.Archive.Tar (
  untar,
  untarWith,
  Entry (..),
  EntryContent (..),
  entryToDirWithoutMode,
) where

import Control.Exception.Safe (MonadThrow, throwM)
import qualified Control.Foldl as L
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Function ((&))
import Data.Functor.Of (Of (..))
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import GHC.Generics (Generic, Generic1)
import GHC.Records (HasField (..))
import qualified Streaming as S
import Streaming.Archive.Tar.Header
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.Posix.Types (FileMode, GroupID, UserID)
import UnliftIO (IOMode (..), MonadUnliftIO, throwString)
import UnliftIO.Directory
import UnliftIO.IO (withFile)

data Entry body = Entry
  { entryPath :: !FilePath
  , mode :: !FileMode
  , userID :: !UserID
  , userName :: !(Maybe String)
  , groupID :: !GroupID
  , groupName :: !(Maybe String)
  , modifiedTime :: !UTCTime
  , content :: !(EntryContent body)
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

data EntryContent body
  = -- | File size and content
    File !Int64 !body
  | Directory
  | -- | Entry size and type flag
    Other !Int64 !TypeFlag
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

entryToDirWithoutMode :: (MonadUnliftIO m) => FilePath -> S.Stream (Of (Entry LBS.ByteString)) m a -> m a
entryToDirWithoutMode dir = S.mapM_ $ \Entry {..} -> do
  when (isAbsolute entryPath) $
    throwString $
      "Absolute path is not supported: " <> entryPath
  case content of
    Directory -> createDirectoryIfMissing True $ dir </> entryPath
    File _size contnt -> do
      createDirectoryIfMissing True $ dir </> takeDirectory entryPath
      withFile (dir </> entryPath) WriteMode $ \h ->
        liftIO $ LBS.hPut h contnt
    Other {} -> pure ()

untarWith ::
  (MonadThrow m) =>
  (Entry () -> Q.ByteStream m (Q.ByteStream m a) -> m (Q.ByteStream m a)) ->
  Q.ByteStream m a ->
  m (Q.ByteStream m a)
untarWith k = go
  where
    go !q = do
      (eith, rest) <- parseHeader q
      case eith of
        Left (InvalidHeaderChecksum 256 0 _) -> do
          allZeros :> lo <- L.purely Q.fold ((&&) <$> ((512 ==) <$> L.length) <*> L.all (== 0)) $ Q.splitAt 512 rest
          if allZeros
            then pure lo
            else throwM InvalidEndOfArchive
        Left err -> throwM err
        Right hdr -> do
          let numBlocks = dataBlockCount hdr
          go
            =<< k
              (fromHeader hdr ())
              ( Q.splitAt (512 * numBlocks) rest
                  & Q.splitAt (fromIntegral hdr.size)
                  & Q.copy
                  & Q.length
                  >>= \(len' :> st) -> do
                    when (fromIntegral len' < hdr.size) $
                      lift $
                        throwM $
                          NotEnoughBytes (hdrFullName hdr) hdr.size (fromIntegral len')
                    pure $ Q.mwrap $ Q.effects st
              )

untar :: (MonadThrow m) => Q.ByteStream m a -> S.Stream (Of (Entry LBS.ByteString)) m (Q.ByteStream m a)
untar = go
  where
    go !q = do
      (eith, rest) <- lift $ parseHeader q
      case eith of
        Left (InvalidHeaderChecksum 256 0 _) -> do
          allZeros :> lo <- lift $ L.purely Q.fold ((&&) <$> ((512 ==) <$> L.length) <*> L.all (== 0)) $ Q.splitAt 512 rest
          if allZeros
            then pure lo
            else lift $ throwM InvalidEndOfArchive
        Left err -> lift $ throwM err
        Right hdr -> do
          (resl, lo) <- lift $ readDataBlock hdr rest
          case resl of
            Left err -> lift $ throwM err
            Right payload -> do
              S.yield $ fromHeader hdr payload
              go lo

fromHeader :: Header -> body -> Entry body
fromHeader hdr body =
  let entryPath = T.unpack . T.decodeUtf8 . SBS.fromShort $ hdrFullName hdr
      mode = getField @"mode" hdr
      userID = hdr.uid
      userName = T.unpack . T.decodeUtf8 . SBS.fromShort <$> hdrUserName hdr
      groupID = hdr.gid
      groupName = T.unpack . T.decodeUtf8 . SBS.fromShort <$> hdrGroupName hdr
      modifiedTime = hdrModifiedTime hdr
      content = case hdr.typeflag of
        Regular -> File (fromIntegral hdr.size) body
        Dir -> Directory
        ft -> Other (fromIntegral hdr.size) ft
   in Entry {..}