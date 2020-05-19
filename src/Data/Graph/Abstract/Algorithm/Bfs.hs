module Data.Graph.Abstract.Algorithm.Bfs
    ( bfs, bfsFrom
    , distances, paths
    ) where

import Control.Monad
import Data.Graph.Abstract (Graph)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Algorithm.Bfs as Bfs

{-# INLINE bfsF #-}
bfsF :: (Maybe (a, e) -> v -> a)
     -> Maybe (a, Edge s) -> Vertex s -> Accessor s e v a
bfsF f Nothing v = fmap (f Nothing) (value v)
bfsF f (Just (pred, e)) v = do
    label' <- label e
    value' <- value v
    pure $ f (Just (pred, label')) value'

bfsFrom :: (v -> Bool) -> a -> (Maybe (a, e) -> v -> a) -> Graph e v -> Graph e a
bfsFrom isStart default' f g = execute g $ do
    vs <- vfind isStart
    vgraph =<< Bfs.bfsFrom vs default' (bfsF f)

bfs :: a -> (Maybe (a, e) -> v -> a) -> Graph e v -> Graph e a
bfs default' f g = execute g $ vgraph =<< Bfs.bfs default' (bfsF f)

distances :: (v -> Bool) -> Graph e v -> Graph e (Maybe Int)
distances isStart g = execute g $ vgraph =<< Bfs.distances =<< vfind isStart

paths :: (v -> Bool) -> Graph e v -> Graph e (Maybe (v, e))
paths isStart g = execute g $ do
    paths' <- Bfs.paths =<< vfind isStart
    vgraph =<< vbuild (toPair <=< vget paths')
 where
    toPair Nothing = pure Nothing
    toPair (Just e) = Just <$> ((,) <$> value (source e) <*> label e)
