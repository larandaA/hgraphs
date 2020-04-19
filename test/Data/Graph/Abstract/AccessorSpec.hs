module Data.Graph.Abstract.AccessorSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Data.STRef
import Test.Hspec


liftSTSpec :: Spec
liftSTSpec = describe "liftST" $ do

    it "should work correctly for a pure value" $ do

        let g = GAC.empty
        let x = GAA.execute g $ GAA.liftST (pure "x")

        x `shouldBe` "x"

    it "should work correctly for an impure value" $ do

        let g = GAC.empty
        let x = GAA.execute g $ GAA.liftST $ do {
            x <- newSTRef 0;
            modifySTRef x (+ 42);
            readSTRef x
        }

        x `shouldBe` 42

executeSpec :: Spec
executeSpec = describe "execute" $ do

    it "should return a value of a pure value" $ do

        let g = GAC.empty
        let x = GAA.execute g $ pure "x"

        x `shouldBe` "x"

verticesSpec :: Spec
verticesSpec = describe "vertices" $ do

    it "should return an empty list for an empty graph" $ do

        let g = GAC.empty
        let len = GAA.execute g $ do {
            vs <- GAA.vertices;
            pure (length vs)
        }

        len `shouldBe` 0

    it "should return all the vertices" $ do

        let g = GAC.isolated [1, 3, 5, 9]
        let values = GAA.execute g $ do {
            vs <- GAA.vertices;
            traverse GAA.value vs
        }

        values `shouldMatchList` [1, 3, 5, 9]

valueSpec :: Spec
valueSpec = describe "value" $ do

    it "should return a value of the only vertex in a trivial graph" $ do

        let g = GAC.singleton "x"
        let x = GAA.execute g $ do {
            vs <- GAA.vertices;
            GAA.value (head vs)
        }

        x `shouldBe` "x"

    it "should return different values for different vertices" $ do

        let g = GAC.isolated [1, 3, 5, 9]
        let values = GAA.execute g $ do {
            vs <- GAA.vertices;
            traverse GAA.value vs
        }

        values `shouldMatchList` [1, 3, 5, 9]

successorsSpec :: Spec
successorsSpec = describe "successors" $ do

    it "should return an empty list for the only vertex in a trivial graph" $ do

        let g = GAC.singleton ()
        let numSuccs = GAA.execute g $ do {
            vs <- GAA.vertices;
            succs <- GAA.successors (head vs);
            pure (length succs)
        }

        numSuccs `shouldBe` 0

    it "should return correct successors" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex 0;
            v1 <- GAB.vertex 1;
            v2 <- GAB.vertex 2;

            GAB.edge' v0 v1;
            GAB.edge' v0 v2;
            GAB.edge' v1 v2
        }

        let edges = GAA.execute g $ do {
            vs <- GAA.vertices;
            vals <- traverse GAA.value vs;
            succs <- traverse GAA.successors vs;
            succVals <- traverse (traverse GAA.value) succs;
            pure (concat (zipWith (\v s -> map (\sv -> (v, sv)) s) vals succVals))
        }

        edges `shouldMatchList` [(0, 1), (1, 2), (0, 2)]

degreeSpec :: Spec
degreeSpec = describe "degree" $ do

    it "should be zero for the only vertex in a trivial graph" $ do

        let g = GAC.singleton ()
        let degree = GAA.execute g $ do {
            vs <- GAA.vertices;
            GAA.degree (head vs)
        }

        degree `shouldBe` 0

    it "should return different degrees for all vertices" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge' a b;
            GAB.edge' a c;
            GAB.edge' b c
        }

        let degrees = GAA.execute g $ do {
            vs <- GAA.vertices;
            vals <- traverse GAA.value vs;
            degs <- traverse GAA.degree vs;
            pure (zip vals degs)
        }

        degrees `shouldMatchList` [("a", 2), ("b", 1), ("c", 0)]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor" $ do
    liftSTSpec
    executeSpec
    verticesSpec
    valueSpec
    successorsSpec
    degreeSpec
