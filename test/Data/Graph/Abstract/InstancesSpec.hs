module Data.Graph.Abstract.InstancesSpec (spec) where

import qualified Data.Maybe as M
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Common as GAC
import qualified Data.List as L
import Data.Graph.Abstract.Instances
import Test.Hspec


fmapSpec :: Spec
fmapSpec = describe "fmap" $ do

    it "should preserve empty graph" $ do

        let g = fmap (+ 1) GAC.empty

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should not change graph with id function" $ do

        let g = GA.build $ do {
            a <- GA.vertex "a";
            b <- GA.vertex "b";
            c <- GA.vertex "c";

            GA.edge "e1" b a;
            GA.edge "e2" c a;
            GA.edge "e3" c b
        }
        let g' = fmap id g

        GA.vertices g' `shouldMatchList` ["a", "b", "c"]
        GA.edges g' `shouldMatchList` [("b", "e1", "a"), ("c", "e2", "a"), ("c", "e3", "b")]

    it "should not change graph structure" $ do

        let g = GA.build $ do {
            v0 <- GA.vertex 0;
            v1 <- GA.vertex 1;
            v2 <- GA.vertex 2;

            GA.edge "e1" v1 v0;
            GA.edge "e2" v2 v0;
            GA.edge "e3" v2 v1
        }
        let g' = fmap (+ 1) g

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        GA.edges g' `shouldMatchList` [(2, "e1", 1), (3, "e2", 1), (3, "e3", 2)]

apSpec :: Spec
apSpec = describe "ap" $ do

    it "should return an empty graph if graph of functions is empty" $ do

        let gx = GAC.singleton ()
        let g' = GAC.empty <*> gx

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should return an empty graph if graph of values is empty" $ do

        let gf = GAC.singleton id
        let g' = gf <*> GAC.empty

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should return a graph of all possible combinations" $ do

        let gf = GA.build $ do {
            f0 <- GA.vertex (2 +);
            f1 <- GA.vertex (3 *);

            GA.edge "f01" f0 f1
        }

        let gx = GA.build $ do {
            v0 <- GA.vertex (5 :: Int);
            v1 <- GA.vertex 30;

            GA.edge "v10" v1 v0
        }

        let g' = gf <*> gx

        GA.vertices g' `shouldMatchList` [7, 15, 32, 90]
        GA.edges g' `shouldMatchList`
            [ (7, "f01", 15), (32, "f01", 90)
            , (7, "f01", 90), (32, "f01", 15)
            , (90, "v10", 15), (32, "v10", 7)
            ]

bindSpec :: Spec
bindSpec = describe "bind" $ do

    it "should return an empty graph if function returns an empty graph" $ do

        let g = GAC.singleton ()
        let g' = g >>= (const GAC.empty)

        length (GA.vertices g') `shouldBe` 0
        length (GA.edges g') `shouldBe` 0

    it "should return an empty graph if original graph is empty" $ do

        let g = GAC.empty >>= id

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should return a graph with duplicated vertices" $ do

        let g = GA.build $ do {
            v0 <- GA.vertex 0;
            v1 <- GA.vertex 1;
            v2 <- GA.vertex 2;

            GA.edge "01" v0 v1;
            GA.edge "12" v1 v2
        }

        let f n = GAC.isolated (L.replicate n n)

        let g' = g >>= f

        GA.vertices g' `shouldMatchList` [1, 2, 2]
        GA.edges g' `shouldMatchList` [ (1, "12", 2), (1, "12", 2) ]


foldrSpec :: Spec
foldrSpec = describe "foldr" $ do

    it "should return a default value on an empty graph" $ do

        let g = GAC.empty

        foldr (+) 42 g `shouldBe` 42

    it "should return a number of vertices with sum function" $ do

        let g = GAC.isolated [1, 1, 1, 1, 1]

        foldr (+) 0 g `shouldBe` 5

traverseSpec :: Spec
traverseSpec = describe "traverse" $ do

    it "should return a pure graph if traversed with pure function" $ do

        let g = GAC.isolated [1, 2, 3]
        let mg = traverse pure g

        M.isJust mg `shouldBe` True
        
        let (Just g') = mg

        GA.vertices g' `shouldMatchList` [1, 2, 3]
        length (GA.edges g') `shouldBe` 0

    it "should return nothing if one of the vertices is mapped to nothing" $ do

        let g = GAC.isolated [1, 2, 3]

        let f 2 = Nothing
            f _ = Just ()
        
        M.isNothing (traverse f g) `shouldBe` True


spec :: Spec
spec = describe "Data.Graph.Abstract.Instances" $ do
    fmapSpec
    apSpec
    bindSpec
    foldrSpec
    traverseSpec
