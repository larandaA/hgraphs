module Data.Graph.Abstract.Algorithm.Distance
    ( dijkstra, bford
    ) where

import Data.Graph.Abstract (Graph)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Algorithm.Distance as Distance

dijkstra :: (Num a, Ord a) => (v -> Bool) -> (e -> a) -> Graph e v -> Graph e (Maybe a)
dijkstra isStart cost g = execute g $ do
    vs <- vfind isStart
    vgraph =<< Distance.dijkstra vs cost

bford :: (Num a, Ord a) => (v -> Bool) -> (e -> a) -> Graph e v -> Maybe (Graph e (Maybe a))
bford isStart cost g = execute g $ do
    vs <- vfind isStart
    mdists <- Distance.bford vs cost
    case mdists of
        Nothing -> pure Nothing
        (Just dists) -> Just <$> vgraph dists
