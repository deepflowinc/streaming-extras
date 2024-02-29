{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Streaming.Archive.Zip (zipStream) where

import Codec.Archive.Zip.Conduit.Zip (ZipEntry (..), ZipOptions)
import qualified Codec.Archive.Zip.Conduit.Zip as Z
import qualified Conduit as C
import Control.Exception.Safe (MonadThrow)
import Control.Monad (void)
import Control.Monad.Primitive
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.Bifunctor as Bi
import Data.Function ((&))
import Data.Functor.Of (Of)
import qualified Streaming as S
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S

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
