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

{-# INLINE newReserveWithLess #-}
newReserveWithLess :: (a -> a -> Bool) -> Int -> Accessor s e v (PQueue s a)
newReserveWithLess f n = liftST (QM.newReserveWithLess f n)

{-# INLINE newWithLess #-}
newWithLess :: (a -> a -> Bool) -> Accessor s e v (PQueue s a)
newWithLess f = liftST (QM.newWithLess f)

{-# INLINE newReserveWithComp #-}
newReserveWithComp :: (a -> a -> Ordering) -> Int -> Accessor s e v (PQueue s a)
newReserveWithComp f n = liftST (QM.newReserveWithComp f n)

{-# INLINE newWithComp #-}
newWithComp :: (a -> a -> Ordering) -> Accessor s e v (PQueue s a)
newWithComp f = liftST (QM.newWithComp f)

{-# INLINE newReserve #-}
newReserve :: Ord a => Int -> Accessor s e v (PQueue s a)
newReserve n = liftST (QM.newReserve n)

{-# INLINE new #-}
new :: Ord a => Accessor s e v (PQueue s a)
new = liftST QM.new

{-# INLINE empty #-}
empty :: PQueue s a -> Accessor s e v Bool
empty q = liftST (QM.empty q)

{-# INLINE capacity #-}
capacity :: PQueue s a -> Accessor s e v Int
capacity q = liftST (QM.capacity q)

{-# INLINE size #-}
size :: PQueue s a -> Accessor s e v Int
size q = liftST (QM.size q)

{-# INLINE full #-}
full :: PQueue s a -> Accessor s e v Bool
full q = liftST (QM.full q)

{-# INLINE pop #-}
pop :: PQueue s a -> Accessor s e v a
pop q = liftST (QM.pop q)

{-# INLINE push #-}
push :: PQueue s a -> a -> Accessor s e v ()
push q x = liftST (QM.push q x)

{-# INLINE drain #-}
drain :: PQueue s a -> Accessor s e v [a]
drain q = liftST (QM.drain q)
