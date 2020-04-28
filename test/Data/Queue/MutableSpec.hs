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


drainSpec :: Spec
drainSpec = describe "drain" $ do

    it "should create empty list" $ do

        let list = runST $ do {
            q <- Q.new;
            Q.drain q
        }

        list `shouldBe` ([] :: [Int])

    it "should create empty list out of used queue" $ do

        let list = runST $ do {
            q <- Q.new;
            Q.push q 42;
            Q.pop q;
            Q.drain q
        }

        list `shouldBe` []

    it "should return list with elements in correct order" $ do

        let list = runST $ do {
            q <- Q.new;
            Q.push q 2;
            Q.push q 42;
            Q.push q 7;
            Q.drain q
        }

        list `shouldBe` [2, 42, 7]

    it "should return list with elements in correct order after pops" $ do

        let list = runST $ do {
            q <- Q.newReserve 1;
            Q.push q 2;
            Q.push q 42;
            Q.push q 7;
            Q.pop q;
            Q.pop q;
            Q.push q 1;
            Q.pop q;
            Q.push q 5;
            Q.push q 6;

            Q.drain q
        }

        list `shouldBe` [1, 5, 6]


spec :: Spec
spec = describe "Data.Queue.Mutable" $ do
    newSpec
    newReserveSpec
    drainSpec
