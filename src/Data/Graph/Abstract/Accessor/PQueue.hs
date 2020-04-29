module Data.Graph.Abstract.Accessor.PQueue
    ( PQueue
    , newReserve, new
    , newReserveWithLess, newWithLess
    , newReserveWithComp, newWithComp
    , empty, full
    , capacity, size
    , pop, push, drain
    ) where

import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor (Accessor, liftST)
import qualified Data.PriorityQueue.Mutable as QM

type PQueue = QM.STPriorityQueue

newReserveWithLess :: (a -> a -> Bool) -> Int -> Accessor s e v (PQueue s a)
newReserveWithLess f n = liftST (QM.newReserveWithLess f n)

newWithLess :: (a -> a -> Bool) -> Accessor s e v (PQueue s a)
newWithLess f = liftST (QM.newWithLess f)

newReserveWithComp :: (a -> a -> Ordering) -> Int -> Accessor s e v (PQueue s a)
newReserveWithComp f n = liftST (QM.newReserveWithComp f n)

newWithComp :: (a -> a -> Ordering) -> Accessor s e v (PQueue s a)
newWithComp f = liftST (QM.newWithComp f)

newReserve :: Ord a => Int -> Accessor s e v (PQueue s a)
newReserve n = liftST (QM.newReserve n)

new :: Ord a => Accessor s e v (PQueue s a)
new = liftST QM.new

empty :: PQueue s a -> Accessor s e v Bool
empty q = liftST (QM.empty q)

capacity :: PQueue s a -> Accessor s e v Int
capacity q = liftST (QM.capacity q)

size :: PQueue s a -> Accessor s e v Int
size q = liftST (QM.size q)

full :: PQueue s a -> Accessor s e v Bool
full q = liftST (QM.full q)

pop :: PQueue s a -> Accessor s e v a
pop q = liftST (QM.pop q)

push :: PQueue s a -> a -> Accessor s e v ()
push q x = liftST (QM.push q x)

drain :: PQueue s a -> Accessor s e v [a]
drain q = liftST (QM.drain q)
