{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Streaming.Archive.Lzma (
  decompress_,
  decompress,
  decompressWith,
  Lzma.defaultDecompressParams,
  Lzma.decompressTellNoCheck,
  Lzma.decompressTellUnsupportedCheck,
  Lzma.decompressTellAnyCheck,
  Lzma.decompressConcatenated,
  Lzma.decompressAutoDecoder,
  Lzma.decompressMemLimit,
  Lzma.LzmaRet (..),
) where

import Codec.Compression.Lzma (LzmaRet (..))
import Codec.Compression.Lzma qualified as Lzma
import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad.Primitive (PrimMonad, primToPrim)
import Control.Monad.Trans.Class
import Data.ByteString qualified as BS
import Streaming.ByteString qualified as Q

newtype LzmaException = LzmaErrorCode LzmaRet
  deriving stock (Show)
  deriving anyclass (Exception)

decompress_ :: (PrimMonad m, MonadThrow m) => Q.ByteStream m r -> Q.ByteStream m r
decompress_ q = do
  mq <- decompress q
  lift do
    isNull <- Q.null_ mq
    if isNull
      then Q.effects mq
      else throwM Lzma.LzmaRetStreamEnd

decompress :: (PrimMonad m, MonadThrow m) => Q.ByteStream m r -> Q.ByteStream m (Q.ByteStream m r)
decompress =
  decompressWith
    Lzma.defaultDecompressParams
      { Lzma.decompressConcatenated = False
      }

decompressWith ::
  (MonadThrow m, PrimMonad m) =>
  Lzma.DecompressParams ->
  Q.ByteStream m r ->
  -- | Retrurns the remaining stream
  Q.ByteStream m (Q.ByteStream m r)
decompressWith params q0 = go q0 =<< lift (primToPrim (Lzma.decompressST params))
  where
    go stream enc@(Lzma.DecompressInputRequired cont) = do
      lift (Q.unconsChunk stream) >>= \case
        Right (ibs, stream')
          | BS.null ibs -> go stream' enc -- should not happen
          | otherwise -> go stream' =<< lift (primToPrim $ cont ibs)
        Left r -> go (pure r) =<< lift (primToPrim $ cont BS.empty)
    go stream (Lzma.DecompressOutputAvailable obs cont) = do
      Q.chunk obs
      go stream =<< lift (primToPrim cont)
    go stream (Lzma.DecompressStreamEnd leftover) =
      pure (Q.chunk leftover >> stream)
    go _stream (Lzma.DecompressStreamError ecode) =
      throwM $ LzmaErrorCode ecode
