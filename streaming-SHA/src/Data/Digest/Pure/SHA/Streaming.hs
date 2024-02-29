{-# LANGUAGE BlockArguments #-}

module Data.Digest.Pure.SHA.Streaming (
  sha256Stream,
  sha256Stream_,

  -- * Re-export
  module Data.Digest.Pure.SHA,
) where

import Control.Arrow ((>>>))
import qualified Control.Foldl as L
import Data.Binary.Get (pushChunk)
import qualified Data.ByteString as BS
import Data.Digest.Pure.SHA
import Data.Functor.Of (Of (..))
import qualified Streaming.ByteString as Q

{-
>>> :set -XOverloadedStrings
>>> sha256Stream_ "This is README" :: IO (Digest SHA256State)
31232efeb2238573931338e5014c359b34710b4380b9ff4c734242c87979cf0a

>>> sha256 "This is README"
31232efeb2238573931338e5014c359b34710b4380b9ff4c734242c87979cf0a
-}

sha256Stream :: (Monad m) => Q.ByteStream m a -> m (Of (Digest SHA256State) a)
{-# INLINE sha256Stream #-}
sha256Stream =
  L.purely Q.chunkFold $
    completeSha256Incremental
      <$> L.Fold pushChunk sha256Incremental id
      <*> L.premap BS.length L.sum

sha256Stream_ ::
  (Monad m) =>
  Q.ByteStream m a ->
  m (Digest SHA256State)
{-# INLINE sha256Stream_ #-}
sha256Stream_ = sha256Stream >>> fmap \(a :> _) -> a
