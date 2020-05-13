{-# LANGUAGE RankNTypes #-}

module Data.Graph.Abstract.Accessor.Algorithm.MstSpec (spec) where

import Control.Monad
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.Mst as Mst
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec

type Mst s e v = (e -> Int) -> GAA.Accessor s e v [GAA.Edge s]

mstSpec :: (forall s e v. Mst s e v) -> String -> Spec
mstSpec algorithm name = describe name $ do

    it "should return empty list on empty graph" $ do

        let g = GAC.empty

        let (edges, weight) = GAA.execute g $ do {
            mst <- algorithm id;
            labels <- traverse (GAA.label) mst;
            pure (labels, foldr (+) 0 labels)
        }

        weight `shouldBe` 0
        length edges `shouldBe` 0

    it "should return empty list on graph without edges" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let (edges, weight) = GAA.execute g $ do {
            mst <- algorithm id;
            labels <- traverse (GAA.label) mst;
            pure (labels, foldr (+) 0 labels)
        }

        weight `shouldBe` 0
        length edges `shouldBe` 0

    it "should return all edges on a tree" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();
            c <- GAB.vertex ();
            d <- GAB.vertex ();
            e <- GAB.vertex ();

            GAB.edge "ab" a b;
            GAB.edge "ab" b a;
            GAB.edge "bc" b c;
            GAB.edge "bc" c b;
            GAB.edge "cd" c d;
            GAB.edge "cd" d c;
            GAB.edge "ce" c e;
            GAB.edge "ce" e c;
        }
        let cost = const 1

        let (edges, weight) = GAA.execute g $ do {
            mst <- algorithm cost;
            weights <- traverse (fmap cost . GAA.label) mst;
            labels <- traverse (GAA.label) mst;
            pure (labels, foldr (+) 0 weights)
        }

        weight `shouldBe` 4
        edges `shouldMatchList` ["ab", "bc", "cd", "ce"]

    it "should return all edges on a forest" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();
            c <- GAB.vertex ();
            d <- GAB.vertex ();
            e <- GAB.vertex ();
            f <- GAB.vertex ();

            GAB.edge "ab" a b;
            GAB.edge "ab" b a;
            GAB.edge "bf" b f;
            GAB.edge "bf" f b;
            GAB.edge "cd" c d;
            GAB.edge "cd" d c;
            GAB.edge "ce" c e;
            GAB.edge "ce" e c;
        }
        let cost = const 1

        let (edges, weight) = GAA.execute g $ do {
            mst <- algorithm cost;
            weights <- traverse (fmap cost . GAA.label) mst;
            labels <- traverse (GAA.label) mst;
            pure (labels, foldr (+) 0 weights)
        }

        weight `shouldBe` 4
        edges `shouldMatchList` ["ab", "bf", "cd", "ce"]

    it "should return the lightest edges in a full graph" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();
            c <- GAB.vertex ();
            d <- GAB.vertex ();

            GAB.edge ("ab", 1) a b;
            GAB.edge ("ab", 1) b a;
            GAB.edge ("bc", 3) b c;
            GAB.edge ("bc", 3) c b;
            GAB.edge ("cd", 3) c d;
            GAB.edge ("cd", 3) d c;
            GAB.edge ("da", 2) d a;
            GAB.edge ("da", 2) a d;
            GAB.edge ("ca", 1) c a;
            GAB.edge ("ca", 1) a c;
            GAB.edge ("bd", 5) d b;
            GAB.edge ("bd", 5) b d;
        }
        let cost = snd

        let (edges, weight) = GAA.execute g $ do {
            mst <- algorithm cost;
            weights <- traverse (fmap cost . GAA.label) mst;
            labels <- traverse (fmap fst . GAA.label) mst;
            pure (labels, foldr (+) 0 weights)
        }

        weight `shouldBe` 4
        edges `shouldMatchList` ["ab", "da", "ca"]

    it "should include the heaviest edge since it is incident to 1-degree vertex" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();
            c <- GAB.vertex ();
            d <- GAB.vertex ();

            GAB.edge ("ab", 1) a b;
            GAB.edge ("ab", 1) b a;
            GAB.edge ("bc", 3) b c;
            GAB.edge ("bc", 3) c b;
            GAB.edge ("ca", 1) c a;
            GAB.edge ("ca", 1) a c;
            GAB.edge ("bd", 5) d b;
            GAB.edge ("bd", 5) b d;
        }
        let cost = snd

        let (edges, weight) = GAA.execute g $ do {
            mst <- algorithm cost;
            weights <- traverse (fmap cost . GAA.label) mst;
            labels <- traverse (fmap fst . GAA.label) mst;
            pure (labels, foldr (+) 0 weights)
        }

        weight `shouldBe` 7
        edges `shouldMatchList` ["ab", "bd", "ca"]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.Mst" $ do
    mstSpec Mst.kruskal "kruskal"
    mstSpec Mst.prim "prim"
