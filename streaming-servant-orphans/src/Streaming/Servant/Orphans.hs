{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streaming.Servant.Orphans () where

import Control.Monad.Morph (hoist)
import qualified Data.ByteString as BS
import Data.Function (fix)
import Data.Functor.Of
import Servant.API.Stream
import Servant.Types.SourceT (SourceT (..))
import qualified Servant.Types.SourceT as SV
import qualified Streaming as S
import qualified Streaming.ByteString as Q
import qualified Streaming.ByteString.Internal as Q
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
import UnliftIO (MonadIO (..))
import UnliftIO.Exception (throwString)

instance
  (MonadIO m, chunk ~ BS.ByteString) =>
  FromSourceIO chunk (Q.ByteStream m ())
  where
  {-# INLINE fromSourceIO #-}
  fromSourceIO (hoist liftIO -> SourceT f) = f $ fix $ \go -> \case
    SV.Stop -> Q.Empty ()
    SV.Error e -> throwString e
    SV.Skip s -> go s
    SV.Yield a x -> Q.Chunk a $ go x
    SV.Effect m -> Q.Go $ Q.effects $ go <$> m

instance
  (m ~ IO, chunk ~ BS.ByteString) =>
  ToSourceIO chunk (Q.ByteStream m ())
  where
  {-# INLINE toSourceIO #-}
  toSourceIO =
    SV.fromStepT . fix \go -> \case
      Q.Empty () -> SV.Stop
      Q.Chunk bs xs -> SV.Yield bs $ go xs
      Q.Go m -> SV.Effect $ go <$> m

instance (MonadIO m, b ~ a) => FromSourceIO b (S.Stream (Of a) m ()) where
  {-# INLINE fromSourceIO #-}
  fromSourceIO (hoist liftIO -> SourceT f) = f $ fix $ \go -> \case
    SV.Stop -> S.Return ()
    SV.Error e -> throwString e
    SV.Skip s -> go s
    SV.Yield a x -> S.Step (a :> go x)
    SV.Effect m -> S.Effect $ S.effects $ go <$> m

instance (m ~ IO, b ~ a) => ToSourceIO b (S.Stream (Of a) m ()) where
  {-# INLINE toSourceIO #-}
  toSourceIO =
    SV.fromStepT . fix \go -> \case
      S.Step (a :> s) -> SV.Yield a $ go s
      S.Effect m -> SV.Effect $ go <$> m
      S.Return () -> SV.Stop
