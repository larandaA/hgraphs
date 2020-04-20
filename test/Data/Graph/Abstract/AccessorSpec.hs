module Data.Graph.Abstract.AccessorSpec (spec) where

import Control.Monad
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

edgesSpec :: Spec
edgesSpec = describe "edges" $ do

    it "should return an empty list for a graph with isolated vertices" $ do

        let g = GAC.isolated [1..5]
        let len = GAA.execute g $ do {
            es <- GAA.edges;
            pure (length es)
        }

        len `shouldBe` 0

    it "should return all the edges" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge' a b;
            GAB.edge' a c;
            GAB.edge' b c
        }

        let edges = GAA.execute g $ do {
            es <- GAA.edges;
            sourceVals <- traverse GAA.value (map GAA.source es);
            targets <- traverse GAA.target es;
            targetVals <- traverse GAA.value targets;
            pure (zip sourceVals targetVals)
        }

        edges `shouldMatchList` [("a", "b"), ("a", "c"), ("b", "c")]

outgoingSpec :: Spec
outgoingSpec = describe "outgoing" $ do

    it "should return an empty list for the only vertex in a trivial graph" $ do

        let g = GAC.singleton ()
        let numEdges = GAA.execute g $ do {
            vs <- GAA.vertices;
            outs <- GAA.outgoing (head vs);
            pure (length outs)
        }

        numEdges `shouldBe` 0

    it "should return correct outgoing edges" $ do

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
            outs <- traverse GAA.outgoing vs;
            targets <- traverse (traverse GAA.target) outs;
            targetVals <- traverse (traverse GAA.value) targets;
            sourceVals <- traverse (traverse (GAA.value . GAA.source)) outs;
            pure (zip (concat sourceVals) (concat targetVals))
        }

        edges `shouldMatchList` [(0, 1), (1, 2), (0, 2)]

labelSpec :: Spec
labelSpec = describe "label" $ do

    it "should return a label of the only edge in a graph" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";

            GAB.edge "ab" a b
        }

        let label = GAA.execute g $ do {
            es <- GAA.edges;
            GAA.label (head es)
        }

        label `shouldBe` "ab"

    it "should return correct labels for all the edges" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge "ab" a b;
            GAB.edge "ac" a c;
            GAB.edge "bc" b c
        }

        let edges = GAA.execute g $ do {
            es <- GAA.edges;
            sourceVals <- traverse GAA.value (map GAA.source es);
            targets <- traverse GAA.target es;
            targetVals <- traverse GAA.value targets;
            labels <- traverse GAA.label es;
            pure (zip3 sourceVals labels targetVals)
        }

        edges `shouldMatchList` [("a", "ab", "b"), ("a", "ac", "c"), ("b", "bc", "c")]

targetSpec :: Spec
targetSpec = describe "target" $ do

    it "should return a target of the only edge in a graph" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";

            GAB.edge' a b
        }

        let targetValue = GAA.execute g $ do {
            es <- GAA.edges;
            target <- GAA.target (head es);
            GAA.value target
        }

        targetValue `shouldBe` "b"

    it "should return correct targets for all the edges" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge "ab" a b;
            GAB.edge "ac" a c;
            GAB.edge "bc" b c
        }

        let edges = GAA.execute g $ do {
            es <- GAA.edges;
            sourceVals <- traverse GAA.value (map GAA.source es);
            targets <- traverse GAA.target es;
            targetVals <- traverse GAA.value targets;
            labels <- traverse GAA.label es;
            pure (zip3 sourceVals labels targetVals)
        }

        edges `shouldMatchList` [("a", "ab", "b"), ("a", "ac", "c"), ("b", "bc", "c")]

varraySpec :: Spec
varraySpec = describe "varray" $ do

    it "should be filled with default values" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let varrayValues = GAA.execute g $ do {
            vs <- GAA.vertices;
            varr <- GAA.varray 42;
            traverse (GAA.vget varr) vs
        }

        varrayValues `shouldBe` [42, 42, 42, 42]

    it "should change a value for the only vertex in a trivial graph" $ do

        let g = GAC.singleton ()

        let varrayValue = GAA.execute g $ do {
            vs <- GAA.vertices;
            varr <- GAA.varray 42;
            GAA.vset varr (head vs) 4816;
            GAA.vget varr (head vs)
        }

        varrayValue `shouldBe` 4816

    it "should not change a value while setting the same as default value" $ do

        let g = GAC.singleton ()

        let varrayValue = GAA.execute g $ do {
            vs <- GAA.vertices;
            varr <- GAA.varray 42;
            GAA.vset varr (head vs) 42;
            GAA.vget varr (head vs)
        }

        varrayValue `shouldBe` 42

    it "should contain different values for all vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let varrayValues = GAA.execute g $ do {
            vs <- GAA.vertices;
            varr <- GAA.varray 0;
            forM_ vs $ \v -> do {
                val <- GAA.value v;
                GAA.vset varr v (val * val);
            };
            vals <- traverse GAA.value vs;
            varrVals <- traverse (GAA.vget varr) vs;
            pure (zip vals varrVals)
        }

        varrayValues `shouldMatchList` [(1, 1), (2, 4), (3, 9), (4, 16)]

earraySpec :: Spec
earraySpec = describe "earray" $ do

    it "should be filled with default values" $ do

        let g = GAC.path [1, 2, 3, 4]

        let earrayValues = GAA.execute g $ do {
            es <- GAA.edges;
            earr <- GAA.earray 42;
            traverse (GAA.eget earr) es
        }

        earrayValues `shouldBe` [42, 42, 42]

    it "should change a value for the only edge in a graph" $ do

        let g = GAC.path [1, 2]

        let earrayValue = GAA.execute g $ do {
            es <- GAA.edges;
            earr <- GAA.earray 42;
            GAA.eset earr (head es) 4816;
            GAA.eget earr (head es)
        }

        earrayValue `shouldBe` 4816

    it "should not change a value while setting the same as default value" $ do

        let g = GAC.path [1, 2]

        let earrayValue = GAA.execute g $ do {
            es <- GAA.edges;
            earr <- GAA.earray 42;
            GAA.eset earr (head es) 42;
            GAA.eget earr (head es)
        }

        earrayValue `shouldBe` 42

    it "should contain different values for all edges" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();
            c <- GAB.vertex ();

            GAB.edge "ab" a b;
            GAB.edge "ac" a c;
            GAB.edge "bc" b c;
            GAB.edge "cb" c b
        }

        let earrayValues = GAA.execute g $ do {
            es <- GAA.edges;
            earr <- GAA.earray "";
            forM_ es $ \e -> do {
                label <- GAA.label e;
                GAA.eset earr e (label ++ label);
            };
            labels <- traverse GAA.label es;
            earrVals <- traverse (GAA.eget earr) es;
            pure (zip labels earrVals)
        }

        earrayValues `shouldMatchList`
            [ ("ab", "abab"), ("ac", "acac")
            , ("bc", "bcbc"), ("cb", "cbcb")
            ]


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor" $ do
    liftSTSpec
    executeSpec
    verticesSpec
    valueSpec
    successorsSpec
    degreeSpec
    edgesSpec
    outgoingSpec
    labelSpec
    targetSpec
    varraySpec
    earraySpec
