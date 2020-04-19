module Data.Graph.Abstract.AccessorSpec (spec) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Accessor as GAA
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


spec :: Spec
spec = describe "Data.Graph.Abstract.Accessor" $ do
    liftSTSpec
    executeSpec
