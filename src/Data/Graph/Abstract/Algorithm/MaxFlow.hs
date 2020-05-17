module Data.Graph.Abstract.Algorithm.MaxFlow
    ( edkarp, dinic
    , Capacity, Flow
    ) where

import Data.Graph.Abstract (Graph)
import Data.Graph.Abstract.Accessor
import Data.Graph.Abstract.Accessor.Algorithm.MaxFlow (Capacity, Flow)
import qualified Data.Graph.Abstract.Accessor.Algorithm.MaxFlow as MaxFlow

edkarp :: (v -> Bool) -> (v -> Bool) -> (e -> Capacity) -> Graph e v -> Graph Flow v
edkarp isSource isSink capacity g = execute g $ do
    source <- head <$> vfind isSource
    sink <- head <$> vfind isSink
    egraph =<< MaxFlow.edkarp source sink (fmap capacity . label)

dinic :: (v -> Bool) -> (v -> Bool) -> (e -> Capacity) -> Graph e v -> Graph Flow v
dinic isSource isSink capacity g = execute g $ do
    source <- head <$> vfind isSource
    sink <- head <$> vfind isSink
    egraph =<< MaxFlow.dinic source sink (fmap capacity . label)
