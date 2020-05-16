module Data.Graph.Abstract.Accessor.Algorithm.DfsSpec (spec) where

import Control.Monad
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.Dfs as Dfs
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Common as GAC
import Data.Maybe (isJust, isNothing, fromJust)
import Test.Hspec


preorderFromSpec :: Spec
preorderFromSpec = describe "preorderFrom" $ do

    it "should return default values for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            values <- Dfs.preorderFrom [] 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [1, 1, 1, 1]

    it "should return default values for unreachable vertices" $ do

        let g = GAC.isolated [1, 3, 2, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.preorderFrom vs "N" (\_ _ -> pure "Y");
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, "Y"), (2, "Y"), (3, "N"), (4, "N")]

    it "should return distances from start vertex" $ do

        let g = GAC.path ["a", "b", "c", "d"]

        let f Nothing _ = pure 0
            f (Just (dist, _)) _ = pure (dist + 1)

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== "a");
            values <- Dfs.preorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 0), ("b", 1), ("c", 2), ("d", 3)]

    it "should return incremented values of verices" $ do

        let g = GAC.path [1, 2, 3, 4]

        let f _ v = (+ 1) <$> GAA.value v

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            values <- Dfs.preorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 2), (2, 3), (3, 4), (4, 5)]

preorderSpec :: Spec
preorderSpec = describe "preorder" $ do

    it "should work fine with empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            values <- Dfs.preorder 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        length (GA.vertices g') `shouldBe` 0

    it "should return non-default values for all vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.preorder 1 (\_ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [42, 42, 42, 42]

postorderFromSpec :: Spec
postorderFromSpec = describe "postorderFrom" $ do

    it "should return default values for all vertices on empty list of start vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            values <- Dfs.postorderFrom [] 1 (\_ _ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [1, 1, 1, 1]

    it "should return default values for unreachable vertices" $ do

        let g = GAC.isolated [1, 3, 2, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.postorderFrom vs "N" (\_ _ _ -> pure "Y");
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, "Y"), (2, "Y"), (3, "N"), (4, "N")]

    it "should return number of direct edges" $ do

        let g = GAC.path ["a", "b", "c", "d"]

        let f _ es _ = pure (length es)

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== "a");
            values <- Dfs.postorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 1), ("b", 1), ("c", 1), ("d", 0)]

    it "should return back edges description" $ do

        let g = GAC.cycle ["a", "b", "c", "d"]

        let f _ _ [] = pure "No"
            f _ _ [(_, Just _)] = pure "E1"
            f _ _ [(_, Nothing)] = pure "E2"
            f _ _ _ = pure "Else"

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (== "a");
            values <- Dfs.postorderFrom vs "" f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", "No"), ("b", "No"), ("c", "No"), ("d", "E2")]

    it "should return incremented values of verices" $ do

        let g = GAC.path [1, 2, 3, 4]

        let f v _ _ = (+ 1) <$> GAA.value v

        let g' = GAA.execute g $ do {
            vs <- GAA.vertices;
            values <- Dfs.postorderFrom vs (-1) f;
            GAA.vgraph values
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 2), (2, 3), (3, 4), (4, 5)]

postorderSpec :: Spec
postorderSpec = describe "postorder" $ do

    it "should work fine with empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            values <- Dfs.postorder 1 (\_ _ _ -> pure 42);
            GAA.vgraph values
        }

        length (GA.vertices g') `shouldBe` 0

    it "should return non-default values for all vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            vs <- GAA.vfind (< 3);
            values <- Dfs.postorder 1 (\_ _ _ -> pure 42);
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [42, 42, 42, 42]

dffSpec :: Spec
dffSpec = describe "dff" $ do

    it "should work fine with empty graph" $ do

        let g = GAC.empty

        let g' = GAA.execute g $ do {
            preds <- Dfs.dff;
            vs <- GAA.vertices;
            values <- GAA.varray Nothing;
            forM_ vs $ \v -> do {
                pred <- GAA.vget preds v;
                pred' <- traverse (GAA.value . GAA.source) pred;
                GAA.vset values v pred'
            };
            GAA.vgraph values
        }

        length (GA.vertices g') `shouldBe` 0

    it "should return Nothing for isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            preds <- Dfs.dff;
            vs <- GAA.vertices;
            values <- GAA.varray Nothing;
            forM_ vs $ \v -> do {
                pred <- GAA.vget preds v;
                pred' <- traverse (GAA.value . GAA.source) pred;
                GAA.vset values v pred'
            };
            GAA.vgraph values
        }

        GA.vertices g' `shouldMatchList` [Nothing, Nothing, Nothing, Nothing]

    it "should return Nothing only for one vertex in cycle" $ do

        let g = GAC.cycle [1, 2, 3, 4]

        let f Nothing num = num
            f (Just _) num = num + 1

        let numWithParent = GAA.execute g $ do {
            preds <- Dfs.dff;
            GAA.vfold f 0 preds
        }

        numWithParent `shouldBe` 3

acyclicSpec :: Spec
acyclicSpec = describe "acyclic" $ do

    it "should return True for an empty graph" $ do

        let g = GAC.empty

        let acyclic' = GAA.execute g Dfs.acyclic

        acyclic' `shouldBe` True

    it "should return True for a graph with isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let acyclic' = GAA.execute g Dfs.acyclic

        acyclic' `shouldBe` True

    it "should return True for a graph with undirected cycle" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v0 v2;
            GAB.edge' v1 v2
        }

        let acyclic' = GAA.execute g Dfs.acyclic

        acyclic' `shouldBe` True

    it "should return False for a cycle" $ do

        let g = GAC.cycle [1, 2, 3, 4]

        let acyclic' = GAA.execute g Dfs.acyclic

        acyclic' `shouldBe` False

    it "should return False for a graph with cycle not containing all vertices" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v1 v2;
            GAB.edge' v2 v3;
            GAB.edge' v3 v1
        }

        let acyclic' = GAA.execute g Dfs.acyclic

        acyclic' `shouldBe` False

topsortSpec :: Spec
topsortSpec = describe "topsort" $ do

    it "should work fine on an empty graph" $ do

        let g = GAC.empty

        let sorted = GAA.execute g $ do {
            ids <- Dfs.topsort;
            pure (isJust ids)
        }

        sorted `shouldBe` True

    it "should return 0 for the only vertex in a graph" $ do

        let g = GAC.singleton 42

        let mg' = GAA.execute g $ do {
            ids <- Dfs.topsort;
            traverse GAA.vgraph ids
        }

        GA.vertices <$> mg' `shouldBe` (Just [0])

    it "should return the only order for a path" $ do

        let g = GAC.path ["a", "b", "c", "d"]

        let mg' = GAA.execute g $ do {
            ids <- Dfs.topsort;
            traverse GAA.vgraph ids
        }

        isJust mg' `shouldBe` True
        let g' = fromJust mg'

        GA.vertices (GA.zip g g') `shouldMatchList` [("a", 3), ("b", 2), ("c", 1), ("d", 0)]

    it "should return Nothing for a cycle" $ do

        let g = GAC.cycle [1, 2, 3, 4]

        let mg' = GAA.execute g $ do {
            ids <- Dfs.topsort;
            traverse GAA.vgraph ids
        }

        isNothing mg' `shouldBe` True

    it "should not violate property of edges going to a lower index" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();
            v4 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v0 v2;
            GAB.edge' v1 v3;
            GAB.edge' v2 v3;
            GAB.edge' v3 v4;
            GAB.edge' v2 v4
        }

        let f (Just ids) e = do {
            v <- GAA.target e;
            v' <- GAA.vget ids v;
            u' <- GAA.vget ids (GAA.source e);
            pure (v' < u')
        }
            f Nothing _ = pure False

        let sorted = GAA.execute g $ do {
            ids <- Dfs.topsort;
            edges <- GAA.edges;
            correct <- traverse (f ids) edges;
            pure (and correct)
        }

        sorted `shouldBe` True

componentsSpec :: Spec
componentsSpec = describe "components" $ do

    it "should find 0 components in an empty graph" $ do

        let g = GAC.empty
        let compf x c = max (x + 1) c

        let comps = GAA.execute g $ do {
            comp <- Dfs.components;
            GAA.vfold compf 0 comp
        }

        comps `shouldBe` 0

    it "should find 1 component in a singleton" $ do

        let g = GAC.singleton "x"
        let compf x c = max (x + 1) c

        let (comps, g') = GAA.execute g $ do {
            comp <- Dfs.components;
            comps <- GAA.vfold compf 0 comp;
            g' <- GAA.vgraph comp;
            pure (comps, g')
        }

        comps `shouldBe` 1
        GA.vertices (GA.zip g g') `shouldMatchList` [("x", 0)]

    it "should find n components on n isolated vertices" $ do

        let g = GAC.isolated ["a", "b", "c", "d"]
        let compf x c = max (x + 1) c

        let (comps, g') = GAA.execute g $ do {
            comp <- Dfs.components;
            comps <- GAA.vfold compf 0 comp;
            g' <- GAA.vgraph comp;
            pure (comps, g')
        }

        comps `shouldBe` 4
        GA.vertices g' `shouldMatchList` [0, 1, 2, 3]

    it "should find correct components on multiple vertex components" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();
            v4 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v1 v0;

            GAB.edge' v2 v3;
            GAB.edge' v3 v2;
            GAB.edge' v3 v4;
            GAB.edge' v4 v3
        }

        let compf x c = max (x + 1) c
        let f comp e = do {
            v <- GAA.target e;
            c' <- GAA.vget comp v;
            c <- GAA.vget comp (GAA.source e);
            pure (c == c')
        }

        let (comps, correct) = GAA.execute g $ do {
            comp <- Dfs.components;
            comps <- GAA.vfold compf 0 comp;
            edges <- GAA.edges;
            correct <- traverse (f comp) edges;
            pure (comps, and correct)
        }

        comps `shouldBe` 2
        correct `shouldBe` True

bipartiteSpec :: Spec
bipartiteSpec = describe "bipartite" $ do

    it "should return True for an empty graph" $ do

        let g = GAC.empty

        let bipartite' = GAA.execute g Dfs.bipartite

        bipartite' `shouldBe` True

    it "should return True for a graph with isolated vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let bipartite' = GAA.execute g Dfs.bipartite

        bipartite' `shouldBe` True

    it "should return True for a graph with even cycle" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();
            v4 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v1 v0;
            GAB.edge' v1 v2;
            GAB.edge' v2 v1;
            GAB.edge' v2 v3;
            GAB.edge' v3 v2;
            GAB.edge' v3 v4;
            GAB.edge' v4 v3;
            GAB.edge' v4 v1;
            GAB.edge' v1 v4
        }

        let bipartite' = GAA.execute g Dfs.bipartite

        bipartite' `shouldBe` True

    it "should return False for a graph with odd cycle" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v1 v0;
            GAB.edge' v1 v2;
            GAB.edge' v2 v1;
            GAB.edge' v2 v3;
            GAB.edge' v3 v2;
            GAB.edge' v3 v1;
            GAB.edge' v1 v3
        }

        let bipartite' = GAA.execute g Dfs.bipartite

        bipartite' `shouldBe` False

    it "should return True for a graph with bipartite components" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();
            v4 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v1 v0;

            GAB.edge' v2 v3;
            GAB.edge' v3 v2;
            GAB.edge' v3 v4;
            GAB.edge' v4 v3
        }

        let bipartite' = GAA.execute g Dfs.bipartite

        bipartite' `shouldBe` True

    it "should return False for a graph with one non-bipartite component" $ do

        let g = GAB.build $ do {
            v0 <- GAB.vertex ();
            v1 <- GAB.vertex ();
            v2 <- GAB.vertex ();
            v3 <- GAB.vertex ();
            v4 <- GAB.vertex ();

            GAB.edge' v0 v1;
            GAB.edge' v1 v0;

            GAB.edge' v2 v3;
            GAB.edge' v3 v2;
            GAB.edge' v3 v4;
            GAB.edge' v4 v3;
            GAB.edge' v4 v2;
            GAB.edge' v2 v4
        }

        let bipartite' = GAA.execute g Dfs.bipartite

        bipartite' `shouldBe` False


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Algorithm.Dfs" $ do
    preorderFromSpec
    preorderSpec
    postorderFromSpec
    postorderSpec
    dffSpec
    acyclicSpec
    topsortSpec
    componentsSpec
    bipartiteSpec
