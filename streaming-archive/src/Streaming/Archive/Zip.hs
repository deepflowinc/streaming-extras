{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Streaming.Archive.Zip (
  zipStream,

  -- * Re-exports
  ZipOptions (..),
  ZipEntry (..),
  ZipInfo (..),
  defaultZipOptions,
) where

import Codec.Archive.Zip.Conduit.Zip (ZipEntry (..), ZipInfo (..), ZipOptions (..), defaultZipOptions)
import Codec.Archive.Zip.Conduit.Zip qualified as Z
import Conduit qualified as C
import Control.Exception.Safe (MonadThrow)
import Control.Monad (void)
import Control.Monad.Primitive
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor qualified as Bi
import Data.Function ((&))
import Data.Functor.Of (Of)
import Streaming qualified as S
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as S

newtype WrappedByteStream m a = WrapByteStream {unwrapByteStream :: Q.ByteStream m a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadTrans)

instance (PrimMonad m) => PrimMonad (WrappedByteStream m) where
  type PrimState (WrappedByteStream m) = PrimState m
  primitive = lift . primitive

zipStream ::
  (MonadThrow m, C.PrimMonad m) =>
  ZipOptions ->
  S.Stream (Of (ZipEntry, Q.ByteStream m ())) m () ->
  Q.ByteStream m ()
zipStream zopts st =
  unwrapByteStream $
    C.runConduit $
      ( S.hoist (lift . lift) st
          & S.map (Bi.second $ Z.ZipDataSource . Q.chunkMapM_ C.yield . S.hoist (lift . lift))
          & S.mapM_ C.yield
      )
        C..| void (Z.zipStream zopts)
        C..| C.mapM_C (WrapByteStream . Q.fromStrict)
