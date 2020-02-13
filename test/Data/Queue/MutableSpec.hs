module Data.Queue.MutableSpec (spec) where

import Control.Monad.ST
import qualified Data.Queue.Mutable as Q
import Test.Hspec


newSpec :: Spec
newSpec = describe "new" $ do

    it "should create empty queue" $ do

        let isEmpty = runST $ do {
            q <- Q.new;
            Q.empty q
        }

        isEmpty `shouldBe` True


newReserveSpec :: Spec
newReserveSpec = describe "newReserve" $ do

    it "should create empty queue" $ do

        let isEmpty = runST $ do {
            q <- Q.newReserve 5;
            Q.empty q
        }

        isEmpty `shouldBe` True

    it "should create queue of specified capacity" $ do

        let capacity = runST $ do {
            q <- Q.newReserve 42;
            Q.capacity q
        }

        capacity `shouldBe` 42

spec :: Spec
spec = describe "Data.Queue.Mutable" $ do
    newSpec
    newReserveSpec
