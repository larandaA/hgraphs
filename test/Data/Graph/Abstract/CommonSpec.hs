module Data.Graph.Abstract.CommonSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec


emptySpec :: Spec
emptySpec = describe "empty" $ do

    it "should create a graph with 0 vertices" $ do

        let g = GAC.empty

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

isolatedSpec :: Spec
isolatedSpec = describe "isolated" $ do

    it "should create a graph with 0 vertices" $ do

        let g = GAC.isolated []

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create a graph with 1 vertex" $ do

        let g = GAC.isolated ["v"]

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

    it "should create a graph with many vertices and no edges" $ do

        let g = GAC.isolated [1, 2, 3, 4, 5]

        GA.vertices g `shouldMatchList` [1, 2, 3, 4, 5]
        length (GA.edges g) `shouldBe` 0

singletonSpec :: Spec
singletonSpec = describe "singleton" $ do

    it "should create a trivial graph" $ do

        let g = GAC.singleton "v"

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

pathSpec :: Spec
pathSpec = describe "path" $ do

    it "should create a graph with 0 vertices" $ do

        let g = GAC.path []

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create a graph with 1 vertex" $ do

        let g = GAC.path ["v"]

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

    it "should create a path of 2 vertices" $ do

        let g = GAC.path [1, 2]

        GA.vertices g `shouldMatchList` [1, 2]
        GA.edges g `shouldMatchList` [(1, (), 2)]

    it "should create a path of 5 vertices" $ do

        let g = GAC.path [1, 2, 3, 4, 5]

        GA.vertices g `shouldMatchList` [1, 2, 3, 4, 5]
        GA.edges g `shouldMatchList` [(1, (), 2), (2, (), 3), (3, (), 4), (4, (), 5)]

cycleSpec :: Spec
cycleSpec = describe "cycle" $ do

    it "should create a graph with 0 vertices" $ do

        let g = GAC.cycle []

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create a graph with 1 vertex" $ do

        let g = GAC.cycle ["v"]

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

    it "should create a cycle of 2 vertices" $ do

        let g = GAC.cycle [1, 2]

        GA.vertices g `shouldMatchList` [1, 2]
        GA.edges g `shouldMatchList` [(1, (), 2), (2, (), 1)]

    it "should create a cycle of 5 vertices" $ do

        let g = GAC.cycle [1, 2, 3, 4, 5]

        GA.vertices g `shouldMatchList` [1, 2, 3, 4, 5]
        GA.edges g `shouldMatchList` [(1, (), 2), (2, (), 3), (3, (), 4), (4, (), 5), (5, (), 1)]

cliqueSpec :: Spec
cliqueSpec = describe "clique" $ do

    it "should create a graph with 0 vertices" $ do

        let g = GAC.clique []

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create a graph with 1 vertex" $ do

        let g = GAC.clique ["v"]

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

    it "should create a clique of 2 vertices" $ do

        let g = GAC.clique [1, 2]

        GA.vertices g `shouldMatchList` [1, 2]
        GA.edges g `shouldMatchList` [(1, (), 2), (2, (), 1)]

    it "should create a clique of 4 vertices" $ do

        let g = GAC.clique [1, 2, 3, 4]

        GA.vertices g `shouldMatchList` [1, 2, 3, 4]
        GA.edges g `shouldMatchList`
            [ (1, (), 2), (2, (), 1)
            , (1, (), 3), (3, (), 1)
            , (1, (), 4), (4, (), 1)
            , (2, (), 3), (3, (), 2)
            , (2, (), 4), (4, (), 2)
            , (3, (), 4), (4, (), 3)
            ]

bicliqueSpec :: Spec
bicliqueSpec = describe "biclique" $ do

    it "should create a graph with 0 vertices" $ do

        let g = GAC.biclique [] []

        length (GA.vertices g) `shouldBe` 0
        length (GA.edges g) `shouldBe` 0

    it "should create a biclique with 0 vertices in one part and 1 vertex in another" $ do

        let g = GAC.biclique [] ["v"]

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

    it "should create a biclique with 1 vertex in one part and 0 vertices in another" $ do

        let g = GAC.biclique ["v"] []

        GA.vertices g `shouldMatchList` ["v"]
        length (GA.edges g) `shouldBe` 0

    it "should create a biclique with 1 vertex in one part and 1 vertex in another" $ do

        let g = GAC.biclique [1] [2]

        GA.vertices g `shouldMatchList` [1, 2]
        GA.edges g `shouldMatchList` [(1, (), 2)]

    it "should create a biclique with 2 vertices in one part and 3 vertices in another" $ do

        let g = GAC.biclique [1, 2] [3, 4, 5]

        GA.vertices g `shouldMatchList` [1, 2, 3, 4, 5]
        GA.edges g `shouldMatchList`
            [ (1, (), 3), (2, (), 3)
            , (1, (), 4), (2, (), 4)
            , (1, (), 5), (2, (), 5)
            ]

starSpec :: Spec
starSpec = describe "star" $ do

    it "should create a star with 0 rays" $ do

        let g = GAC.star 1 []

        GA.vertices g `shouldMatchList` [1]
        length (GA.edges g) `shouldBe` 0

    it "should create a star with 1 ray" $ do

        let g = GAC.star 1 [2]

        GA.vertices g `shouldMatchList` [1, 2]
        GA.edges g `shouldMatchList` [(1, (), 2)]

    it "should create a star with 3 rays" $ do

        let g = GAC.star 1 [2, 3, 4]

        GA.vertices g `shouldMatchList` [1, 2, 3, 4]
        GA.edges g `shouldMatchList` [(1, (), 2), (1, (), 3), (1, (), 4)]


spec :: Spec
spec = describe "Data.Graph.Abstract.Common" $ do
    emptySpec
    isolatedSpec
    singletonSpec
    pathSpec
    cycleSpec
    cliqueSpec
    bicliqueSpec
    starSpec
