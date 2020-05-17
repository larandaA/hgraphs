module Data.Graph.Abstract.Algorithm.Mst
    ( kruskal, prim
    ) where

import Control.Monad
import Data.Graph.Abstract (Graph)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Algorithm.Mst as Mst

markEdges :: [Edge s] -> Accessor s e v (Graph Bool v)
markEdges es = do
    marks <- earray False
    forM_ es $ \edge -> eset marks edge True
    egraph marks

kruskal :: Ord a => (e -> a) -> Graph e v -> Graph Bool v
kruskal cost g = execute g $ markEdges =<< Mst.kruskal cost

prim :: Ord a => (e -> a) -> Graph e v -> Graph Bool v
prim cost g = execute g $ markEdges =<< Mst.prim cost
