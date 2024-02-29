{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.HTTP.Streaming (
  fromPopper,
  toGivesPopper,
  toGivesPopperWith,
  toGivesPopperUnlifted,
  unliftStream,
  toRequestBody,
  toRequestBodyWith,
  toRequestBodyWithN,
  fromRequestBody,
  fromGivesPopper,
) where

import Control.Arrow ((>>>))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Network.HTTP.Client
import Streaming (hoist)
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import UnliftIO (MonadIO (liftIO), MonadUnliftIO, newEmptyMVar, newIORef, putMVar, readIORef, takeMVar, withRunInIO, writeIORef)

fromRequestBody :: RequestBody -> Q.ByteStream IO ()
fromRequestBody (RequestBodyBS bs) = Q.fromStrict bs
fromRequestBody (RequestBodyLBS lbs) = Q.fromLazy lbs
fromRequestBody (RequestBodyBuilder _ lbs) = Q.toStreamingByteString lbs
fromRequestBody (RequestBodyStream _ pop) = fromGivesPopper pop
fromRequestBody (RequestBodyStreamChunked pop) = fromGivesPopper pop
fromRequestBody (RequestBodyIO io) = Q.mwrap $ fromRequestBody <$> io

fromGivesPopper :: GivesPopper () -> Q.ByteStream IO ()
fromGivesPopper popper = do
  mv <- newEmptyMVar
  liftIO $ popper $ \pop -> putMVar mv pop
  fromPopper =<< takeMVar mv

fromPopper :: (MonadIO m) => Popper -> Q.ByteStream m ()
{-# INLINE fromPopper #-}
fromPopper =
  liftIO
    >>> S.repeatM
    >>> S.takeWhile (not . BS.null)
    >>> Q.fromChunks

unliftStream :: (MonadUnliftIO m) => Q.ByteStream m a -> m (Q.ByteStream IO a)
{-# INLINE unliftStream #-}
unliftStream qbs = withRunInIO $ \runInIO ->
  pure $ hoist runInIO qbs

toGivesPopperUnlifted :: (MonadUnliftIO m) => Q.ByteStream m () -> m (GivesPopper a)
{-# INLINE toGivesPopperUnlifted #-}
toGivesPopperUnlifted bs = withRunInIO $ \runInIO ->
  pure $ toGivesPopperWith runInIO bs

toRequestBody :: (MonadUnliftIO m) => Q.ByteStream m () -> m RequestBody
{-# INLINE toRequestBody #-}
toRequestBody = fmap RequestBodyStreamChunked . toGivesPopperUnlifted

toRequestBodyWith ::
  (Monad m) =>
  (forall x. m x -> IO x) ->
  Q.ByteStream m () ->
  RequestBody
{-# INLINE toRequestBodyWith #-}
toRequestBodyWith = \x -> fmap RequestBodyStreamChunked (toGivesPopperWith x)

toRequestBodyWithN ::
  (Monad m) =>
  (forall x. m x -> IO x) ->
  Int64 ->
  Q.ByteStream m a ->
  RequestBody
{-# INLINE toRequestBodyWithN #-}
toRequestBodyWithN unlift size = RequestBodyStream size . toGivesPopperWith unlift

toGivesPopperWith :: (Monad m) => (forall x. m x -> IO x) -> Q.ByteStream m a -> GivesPopper b
{-# SPECIALIZE toGivesPopperWith ::
  (forall x. IO x -> IO x) -> Q.ByteStream IO () -> GivesPopper a
  #-}
toGivesPopperWith runInIO bs0 f = do
  ref <- newIORef bs0
  let popper = do
        bs <- readIORef ref
        eith <- runInIO $ Q.unconsChunk bs
        case eith of
          Left {} -> pure mempty
          Right (chunk, lo) -> chunk <$ writeIORef ref lo
  f popper

toGivesPopper :: Q.ByteStream IO () -> GivesPopper a
{-# INLINE toGivesPopper #-}
toGivesPopper = toGivesPopperWith id
