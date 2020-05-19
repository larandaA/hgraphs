module Data.Graph.Abstract.Accessor.Queue
    ( Queue
    , newReserve, new
    , empty, full
    , capacity, size
    , pop, push, drain
    ) where

import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor (Accessor, liftST)
import qualified Data.Queue.Mutable as QM

type Queue = QM.STQueue

{-# INLINE newReserve #-}
newReserve :: Int -> Accessor s e v (Queue s a)
newReserve n = liftST (QM.newReserve n)

{-# INLINE new #-}
new :: Accessor s e v (Queue s a)
new = liftST QM.new

{-# INLINE empty #-}
empty :: Queue s a -> Accessor s e v Bool
empty q = liftST (QM.empty q)

{-# INLINE capacity #-}
capacity :: Queue s a -> Accessor s e v Int
capacity q = liftST (QM.capacity q)

{-# INLINE size #-}
size :: Queue s a -> Accessor s e v Int
size q = liftST (QM.size q)

{-# INLINE full #-}
full :: Queue s a -> Accessor s e v Bool
full q = liftST (QM.full q)

{-# INLINE pop #-}
pop :: Queue s a -> Accessor s e v a
pop q = liftST (QM.pop q)

{-# INLINE push #-}
push :: Queue s a -> a -> Accessor s e v ()
push q x = liftST (QM.push q x)

{-# INLINE drain #-}
drain :: Queue s a -> Accessor s e v [a]
drain q = liftST (QM.drain q)
