module Data.Graph.Abstract.Accessor.DsuSpec (spec) where

import Control.Monad
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
import qualified Data.Graph.Abstract.Accessor.Dsu as Dsu
import qualified Data.Graph.Abstract.Common as GAC
import Test.Hspec


dsuSpec :: Spec
dsuSpec = describe "dsu" $ do

    it "should make vertices separate components before unions" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let g' = GAA.execute g $ do {
            dsu <- Dsu.new;
            comps <- GAA.vbuild (GAA.value <=< Dsu.find dsu);
            GAA.vgraph comps
        }

        GA.vertices (GA.zip g g') `shouldMatchList` [(1, 1), (2, 2), (3, 3), (4, 4)]

    it "should keep vertex a root on union of vertex with itself" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let corr = GAA.execute g $ do {
            v1 <- head <$> GAA.vfind (== 1);

            dsu <- Dsu.new;
            Dsu.union dsu v1 v1;

            rv1 <- Dsu.find dsu v1;
            pure (rv1 == v1)
        }

        corr `shouldBe` True

    it "should put vertices into one set when uniting them" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let corr = GAA.execute g $ do {
            v1 <- head <$> GAA.vfind (== 1);
            v4 <- head <$> GAA.vfind (== 4);

            dsu <- Dsu.new;
            Dsu.union dsu v1 v4;

            rv1 <- Dsu.find dsu v1;
            rv4 <- Dsu.find dsu v4;
            pure (rv1 == rv4)
        }

        corr `shouldBe` True

    it "should put vertices into one set when uniting them with united vertices" $ do

        let g = GAC.isolated [1, 2, 3, 4]

        let corr = GAA.execute g $ do {
            v1 <- head <$> GAA.vfind (== 1);
            v2 <- head <$> GAA.vfind (== 2);
            v3 <- head <$> GAA.vfind (== 3);
            v4 <- head <$> GAA.vfind (== 4);

            dsu <- Dsu.new;
            Dsu.union dsu v1 v2;
            Dsu.union dsu v3 v1;
            Dsu.union dsu v4 v2;

            rv3 <- Dsu.find dsu v3;
            rv4 <- Dsu.find dsu v4;
            pure (rv3 == rv4)
        }

        corr `shouldBe` True


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor.Dsu" $ do
    dsuSpec
