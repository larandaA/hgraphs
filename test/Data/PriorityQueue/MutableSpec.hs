module Data.PriorityQueue.MutableSpec (spec) where

import Control.Monad.ST
import qualified Data.PriorityQueue.Mutable as Q
import Test.Hspec

newSpec :: Spec
newSpec = describe "new" $ do

    it "should create empty queue" $ do

        let isEmpty = runST $ do {
            q <- Q.new :: ST s (Q.STPriorityQueue s Int);
            Q.empty q
        }

        isEmpty `shouldBe` True

newReserveSpec :: Spec
newReserveSpec = describe "newReserve" $ do

    it "should create empty queue" $ do

        let isEmpty = runST $ do {
            q <- Q.newReserve 5 :: ST s (Q.STPriorityQueue s Int);
            Q.empty q
        }

        isEmpty `shouldBe` True

    it "should create queue of specified capacity" $ do

        let capacity = runST $ do {
            q <- Q.newReserve 42 :: ST s (Q.STPriorityQueue s Int);
            Q.capacity q
        }

        capacity `shouldBe` 42

newReserveWithLessSpec :: Spec
newReserveWithLessSpec = describe "newReserveWithLess" $ do

    it "should give higher priority to a greater value" $ do

        let maxVal = runST $ do {
            q <- Q.newReserveWithLess (>) 5;
            Q.push q 5;
            Q.push q 2;
            Q.push q 10;
            Q.push q 8;
            Q.pop q
        }

        maxVal `shouldBe` 10

    it "should give higher priority to a less value" $ do

        let minVal = runST $ do {
            q <- Q.newReserveWithLess (<) 5;
            Q.push q 5;
            Q.push q 2;
            Q.push q 10;
            Q.push q 8;
            Q.pop q
        }

        minVal `shouldBe` 2

popSpec :: Spec
popSpec = describe "pop" $ do

    it "should return the minimum value" $ do

        let minVal = runST $ do {
            q <- Q.newReserve 5;
            Q.push q 5;
            Q.push q 2;
            Q.push q 10;
            Q.push q 8;
            Q.pop q
        }

        minVal `shouldBe` 2

    it "should return the second minimum value" $ do

        let minVal = runST $ do {
            q <- Q.newReserve 5;
            Q.push q 5;
            Q.push q 2;
            Q.push q 10;
            Q.push q 8;
            Q.pop q;
            Q.pop q
        }

        minVal `shouldBe` 5

sizeSpec :: Spec
sizeSpec = describe "size" $ do

    it "should be zero for an empty queue" $ do

        let size = runST $ do {
            q <- Q.newReserve 5 :: ST s (Q.STPriorityQueue s Int);
            Q.size q
        }

        size `shouldBe` 0

    it "should be increased after pushing a value" $ do

        let size = runST $ do {
            q <- Q.newReserve 5;
            Q.push q 5;
            Q.size q
        }

        size `shouldBe` 1

    it "should be decreased after popping a value" $ do

        let size = runST $ do {
            q <- Q.newReserve 5;
            Q.push q 5;
            Q.push q 5;
            Q.push q 5;
            Q.pop q;
            Q.size q
        }

        size `shouldBe` 2

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

        list `shouldBe` [2, 7, 42]

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

        list `shouldBe` [5, 6, 42]


spec :: Spec
spec = describe "Data.PriorityQueue.Mutable" $ do
    newSpec
    newReserveSpec
    newReserveWithLessSpec
    popSpec
    sizeSpec
    drainSpec
