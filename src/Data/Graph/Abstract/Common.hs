module Data.Graph.Abstract.Common where

import Data.Graph.Abstract
import qualified Data.List as L

empty :: Graph e v
empty = build $ pure ()

isolated :: [v] -> Graph e v
isolated ls = build $ mapM_ vertex ls

singleton :: v -> Graph e v
singleton l = isolated [l]

path :: [v] -> Graph' v
path [] = empty
path ls = build $ do
    vs <- mapM vertex ls
    mapM (uncurry edge') (L.zip vs (L.tail vs))

cycle :: [v] -> Graph' v
cycle [] = empty
cycle [l] = singleton l
cycle (l:ls) = build $ do
    v <- vertex l
    vs <- mapM vertex ls
    mapM (uncurry edge') (L.zip ([v] ++ vs) (vs ++ [v]))

clique :: [v] -> Graph' v
clique ls = build $ do
    vs <- mapM vertex ls
    let ivs = L.zip [1..] vs
    sequence_ [edge' av bv | (i, av) <- ivs, (j, bv) <- ivs, i /= j]

biclique :: [v] -> [v] -> Graph' v
biclique als bls = build $ do
    avs <- mapM vertex als
    bvs <- mapM vertex bls
    sequence_ [edge' av bv | av <- avs, bv <- bvs]

star :: v -> [v] -> Graph' v
star l = biclique [l]
