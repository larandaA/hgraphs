module Data.Graph.Abstract.Algorithm.Dfs
    ( preorder, preorderFrom
    , postorder, postorderFrom
    , dff
    , acyclic, topsort
    , components, connected
    , bicolour, bipartite
    ) where

import Control.Monad
import Data.Graph.Abstract (Graph)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Algorithm.Dfs as Dfs

{-# INLINE preorderF #-}
preorderF :: (Maybe (a, e) -> v -> a)
          -> Maybe (a, Edge s) -> Vertex s -> Accessor s e v a
preorderF f Nothing v = fmap (f Nothing) (value v)
preorderF f (Just (pred, e)) v = do
    label' <- label e
    value' <- value v
    pure $ f (Just (pred, label')) value'

preorder :: a -> (Maybe (a, e) -> v -> a) -> Graph e v -> Graph e a
preorder default' f g = execute g $ vgraph =<< Dfs.preorder default' (preorderF f)

preorderFrom :: (v -> Bool) -> a -> (Maybe (a, e) -> v -> a) -> Graph e v -> Graph e a
preorderFrom isStart default' f g = execute g $ do
    vs <- vfind isStart
    vgraph =<< Dfs.preorderFrom vs default' (preorderF f)

{-# INLINE postorderF #-}
postorderF :: (v -> [(e, a)] -> [(e, Maybe a)] -> a)
           -> Vertex s -> [(Edge s, a)] -> [(Edge s, Maybe a)] -> Accessor s e v a
postorderF f v children succs = do
    children' <- traverse (\(e, v) -> (,) <$> label e <*> pure v) children
    succs' <- traverse (\(e, mv) -> (,) <$> label e <*> pure mv) succs
    v' <- value v
    pure $ f v' children' succs'

postorder :: a -> (v -> [(e, a)] -> [(e, Maybe a)] -> a) -> Graph e v -> Graph e a
postorder default' f g = execute g $ vgraph =<< Dfs.postorder default' (postorderF f)

postorderFrom :: (v -> Bool) -> a -> (v -> [(e, a)] -> [(e, Maybe a)] -> a) -> Graph e v -> Graph e a
postorderFrom isStart default' f g = execute g $ do
    vs <- vfind isStart
    vgraph =<< Dfs.postorderFrom vs default' (postorderF f)

dff :: Graph e v -> Graph e (Maybe (v, e))
dff g = execute g $ do
    dff' <- Dfs.dff
    vgraph =<< vbuild (toPair <=< vget dff')
 where
    toPair Nothing = pure Nothing
    toPair (Just e) = Just <$> ((,) <$> value (source e) <*> label e)

acyclic :: Graph e v -> Bool
acyclic g = execute g Dfs.acyclic

topsort :: Graph e v -> Maybe (Graph e Int)
topsort g = execute g $ do
    ids <- Dfs.topsort
    case ids of
        Nothing -> pure Nothing
        (Just ids') -> Just <$> vgraph ids'

components :: Graph e v -> Graph e Int
components g = execute g $ vgraph =<< Dfs.components

connected :: Graph e v -> Bool
connected g = execute g Dfs.connected

bicolour :: Graph e v -> Graph e Int
bicolour g = execute g $ vgraph =<< Dfs.bicolour

bipartite :: Graph e v -> Bool
bipartite g = execute g Dfs.bipartite
