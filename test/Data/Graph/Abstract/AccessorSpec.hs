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

    it "should return an empty graph for an empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            varr <- GAA.varray 42;
            GAA.vgraph varr
        }

        length(GA.vertices g') `shouldBe` 0

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

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            varr <- GAA.varray 0;
            forM_ vs $ \v -> do {
                val <- GAA.value v;
                GAA.vset varr v (val * val);
            };
            GAA.vgraph varr
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 1), (2, 4), (3, 9), (4, 16)]

earraySpec :: Spec
earraySpec = describe "earray" $ do

    it "should return graph without edges for graph without edges" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            earr <- GAA.earray 42;
            GAA.egraph earr
        }

        length(GA.edges g') `shouldBe` 0

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
            a <- GAB.vertex "a";
            b <- GAB.vertex "b";
            c <- GAB.vertex "c";

            GAB.edge "ab" a b;
            GAB.edge "ac" a c;
            GAB.edge "bc" b c;
            GAB.edge "cb" c b
        }

        let g' = GAA.execute g $ do {
            es <- GAA.edges;
            earr <- GAA.earray "";
            forM_ es $ \e -> do {
                label <- GAA.label e;
                GAA.eset earr e (label ++ label);
            };
            GAA.egraph earr
        }

        GA.edges (GA.zip g g') `shouldMatchList`
            [ (("a", "a"), ("ab", "abab"), ("b", "b"))
            , (("a", "a"), ("ac", "acac"), ("c", "c"))
            , (("b", "b"), ("bc", "bcbc"), ("c", "c"))
            , (("c", "c"), ("cb", "cbcb"), ("b", "b"))
            ]

vfoldSpec :: Spec
vfoldSpec = describe "vfold" $ do

    it "should return a default value on empty graph" $ do

        let g = GAC.empty

        let sum = GAA.execute g $ do {
            varr <- GAA.varray 42;
            GAA.vfold (+) 0 varr
        }

        sum `shouldBe` 0

    it "should return a number of vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let sum = GAA.execute g $ do {
            varr <- GAA.varray 1;
            GAA.vfold (+) 0 varr
        }

        sum `shouldBe` 4

efoldSpec :: Spec
efoldSpec = describe "efold" $ do

    it "should return a default value on empty graph" $ do

        let g = GAC.empty

        let sum = GAA.execute g $ do {
            earr <- GAA.earray 42;
            GAA.efold (+) 0 earr
        }

        sum `shouldBe` 0


    it "should return a default value on graph without edges" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let sum = GAA.execute g $ do {
            earr <- GAA.earray 42;
            GAA.efold (+) 0 earr
        }

        sum `shouldBe` 0

    it "should return a number of edges" $ do

        let g = GAC.path [1, 2, 3, 4]

        let sum = GAA.execute g $ do {
            earr <- GAA.earray 1;
            GAA.efold (+) 0 earr
        }

        sum `shouldBe` 3

vbuildSpec :: Spec
vbuildSpec = describe "vbuild" $ do

    it "should work fine on empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            varr <- GAA.vbuild (const (pure 42));
            GAA.vgraph varr
        }

        GA.numVertices g' `shouldBe` 0

    it "should fill with constant value" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            varr <- GAA.vbuild (const (pure 42));
            GAA.vgraph varr
        }

        GA.vertices g' `shouldMatchList` [42, 42, 42, 42]

    it "should fill with incremented vertices values" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            varr <- GAA.vbuild (fmap (+ 1) . GAA.value);
            GAA.vgraph varr
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 2), (2, 3), (3, 4), (4, 5)]

ebuildSpec :: Spec
ebuildSpec = describe "ebuild" $ do

    it "should work fine on empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            earr <- GAA.ebuild (const (pure 42));
            GAA.egraph earr
        }

        length (GA.edges g') `shouldBe` 0

    it "should work fine on graph without edges" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            earr <- GAA.ebuild (const (pure 42));
            GAA.egraph earr
        }

        length (GA.edges g') `shouldBe` 0

    it "should fill with constant value" $ do

        let g = GAC.path [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            earr <- GAA.ebuild (const (pure 42));
            GAA.egraph earr
        }

        GA.edges g' `shouldMatchList` [(1, 42, 2), (2, 42, 3), (3, 42, 4)]

    it "should fill with incremented source values" $ do

        let g = GAC.path [4, 3, 2, 1]

        let g' = GAA.execute g $ do {
            earr <- GAA.ebuild (fmap (+ 1) . GAA.value . GAA.source);
            GAA.egraph earr
        }

        GA.edges g' `shouldMatchList` [(4, 5, 3), (3, 4, 2), (2, 3, 1)]

vfindSpec :: Spec
vfindSpec = describe "vfind" $ do

    it "should return an empty list for an empty graph" $ do

        let g = GAC.empty

        let vertVals = GAA.execute g $ do {
            vs <- GAA.vfind (const True);
            traverse GAA.value vs
        }

        length vertVals `shouldBe` 0

    it "should return an empty list with a constantly false predicate" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let vertVals = GAA.execute g $ do {
            vs <- GAA.vfind (const False);
            traverse GAA.value vs
        }

        length vertVals `shouldBe` 0

    it "should return all vertices with a constantly true predicate" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let vertVals = GAA.execute g $ do {
            vs <- GAA.vfind (const True);
            traverse GAA.value vs
        }

        vertVals `shouldMatchList` [1, 2, 3, 4]

    it "should return only one vertex" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let vertVals = GAA.execute g $ do {
            vs <- GAA.vfind (== 3);
            traverse GAA.value vs
        }

        vertVals `shouldMatchList` [3]

    it "should return only vertices with positive values" $ do

        let g = GAC.isolated [-2, 4, 42, -42, 4816]

        let vertVals = GAA.execute g $ do {
            vs <- GAA.vfind (> 0);
            traverse GAA.value vs
        }

        vertVals `shouldMatchList` [4, 42, 4816]

efindSpec :: Spec
efindSpec = describe "efind" $ do

    it "should return an empty list for an empty graph" $ do

        let g = GAC.empty

        let edgeLabels = GAA.execute g $ do {
            es <- GAA.efind (const True);
            traverse GAA.label es
        }

        length edgeLabels `shouldBe` 0

    it "should return an empty list for a graph with isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3]

        let edgeLabels = GAA.execute g $ do {
            es <- GAA.efind (const True);
            traverse GAA.label es
        }

        length edgeLabels `shouldBe` 0

    it "should return an empty list with a constantly false predicate" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();

            GAB.edge 1 a b;
            GAB.edge 2 a b;
            GAB.edge 3 a b;
            GAB.edge 4 a b
        }

        let edgeLabels = GAA.execute g $ do {
            es <- GAA.efind (const False);
            traverse GAA.label es
        }

        length edgeLabels `shouldBe` 0

    it "should return all edges with a constantly true predicate" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();

            GAB.edge 1 a b;
            GAB.edge 2 a b;
            GAB.edge 3 a b;
            GAB.edge 4 a b
        }

        let edgeLabels = GAA.execute g $ do {
            es <- GAA.efind (const True);
            traverse GAA.label es
        }

        edgeLabels `shouldMatchList` [1, 2, 3, 4]

    it "should return only one edge" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();

            GAB.edge 1 a b;
            GAB.edge 2 a b;
            GAB.edge 3 a b;
            GAB.edge 4 a b
        }

        let edgeLabels = GAA.execute g $ do {
            es <- GAA.efind (== 3);
            traverse GAA.label es
        }

        edgeLabels `shouldMatchList` [3]

    it "should return only edges with positive labels" $ do

        let g = GAB.build $ do {
            a <- GAB.vertex ();
            b <- GAB.vertex ();

            GAB.edge (-3) a b;
            GAB.edge (-42) a b;
            GAB.edge 42 a b;
            GAB.edge 5 a b;
            GAB.edge 0 a b
        }

        let edgeLabels = GAA.execute g $ do {
            es <- GAA.efind (> 0);
            traverse GAA.label es
        }

        edgeLabels `shouldMatchList` [42, 5]


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
    vfoldSpec
    efoldSpec
    vbuildSpec
    ebuildSpec
    vfindSpec
    efindSpec
