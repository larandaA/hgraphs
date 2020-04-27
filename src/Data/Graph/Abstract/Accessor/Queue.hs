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

newReserve :: Int -> Accessor s e v (Queue s a)
newReserve n = liftST (QM.newReserve n)

new :: Accessor s e v (Queue s a)
new = liftST QM.new

empty :: Queue s a -> Accessor s e v Bool
empty q = liftST (QM.empty q)

capacity :: Queue s a -> Accessor s e v Int
capacity q = liftST (QM.capacity q)

size :: Queue s a -> Accessor s e v Int
size q = liftST (QM.size q)

full :: Queue s a -> Accessor s e v Bool
full q = liftST (QM.full q)

pop :: Queue s a -> Accessor s e v a
pop q = liftST (QM.pop q)

push :: Queue s a -> a -> Accessor s e v ()
push q x = liftST (QM.push q x)

drain :: Queue s a -> Accessor s e v [a]
drain q = liftST (QM.drain q)
