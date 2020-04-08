module Data.Graph.Abstract.Common
    ( empty, isolated, singleton
    , path, cycle
    , clique, biclique, star
    ) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.List as L
import Prelude hiding (cycle)

empty :: GA.Graph e v
empty = GAB.build $ pure ()

isolated :: [v] -> GA.Graph e v
isolated ls = GAB.build $ mapM_ GAB.vertex ls

singleton :: v -> GA.Graph e v
singleton l = isolated [l]

path :: [v] -> GA.Graph' v
path [] = empty
path ls = GAB.build $ do
    vs <- mapM GAB.vertex ls
    mapM (uncurry GAB.edge') (L.zip vs (L.tail vs))

cycle :: [v] -> GA.Graph' v
cycle [] = empty
cycle [l] = singleton l
cycle (l:ls) = GAB.build $ do
    v <- GAB.vertex l
    vs <- mapM GAB.vertex ls
    mapM (uncurry GAB.edge') (L.zip ([v] ++ vs) (vs ++ [v]))

clique :: [v] -> GA.Graph' v
clique ls = GAB.build $ do
    vs <- mapM GAB.vertex ls
    let ivs = L.zip [1..] vs
    sequence_ [GAB.edge' av bv | (i, av) <- ivs, (j, bv) <- ivs, i /= j]

biclique :: [v] -> [v] -> GA.Graph' v
biclique als bls = GAB.build $ do
    avs <- mapM GAB.vertex als
    bvs <- mapM GAB.vertex bls
    sequence_ [GAB.edge' av bv | av <- avs, bv <- bvs]

star :: v -> [v] -> GA.Graph' v
star l = biclique [l]
