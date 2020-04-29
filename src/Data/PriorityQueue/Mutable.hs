module Data.PriorityQueue.Mutable
    ( STPriorityQueue
    , newReserveWithLess, newWithLess
    , newReserveWithComp, newWithComp
    , newReserve, new
    , empty, full
    , size, capacity
    , push, pop, drain
    ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Mutable as VM
import Data.STRef

data STPriorityQueue s e = STPriorityQueue
    { qSize :: STRef s Int
    , qData :: STRef s (VM.STVector s e)
    , qLess :: e -> e -> Bool
    }

defaultCapacity = 8

newReserveWithLess :: (e -> e -> Bool) -> Int -> ST s (STPriorityQueue s e)
newReserveWithLess f n = do
    d <- VM.new n
    d' <- newSTRef d
    s <- newSTRef 0
    pure (STPriorityQueue s d' f)

newWithLess :: (e -> e -> Bool) -> ST s (STPriorityQueue s e)
newWithLess f = newReserveWithLess f defaultCapacity

newReserveWithComp :: (e -> e -> Ordering) -> Int -> ST s (STPriorityQueue s e)
newReserveWithComp f = newReserveWithLess (\e1 e2 -> f e1 e2 == LT)

newWithComp :: (e -> e -> Ordering) -> ST s (STPriorityQueue s e)
newWithComp f = newReserveWithComp f defaultCapacity

newReserve :: Ord e => Int -> ST s (STPriorityQueue s e)
newReserve = newReserveWithLess (<)

new :: Ord e => ST s (STPriorityQueue s e)
new = newReserve defaultCapacity

empty :: STPriorityQueue s e -> ST s Bool
empty q = do
    s <- readSTRef (qSize q)
    pure (s == 0)

capacity :: STPriorityQueue s e -> ST s Int
capacity q = do
    d <- readSTRef (qData q)
    return (VM.length d)

size :: STPriorityQueue s e -> ST s Int
size q = do
    s <- readSTRef (qSize q)
    pure s

full :: STPriorityQueue s e -> ST s Bool
full q = do
    s <- size q
    c <- capacity q
    return (s == c)

parent_ :: Int -> Int
parent_ i = (i - 1) `div` 2

left_ :: Int -> Int
left_ i = 2 * i + 1

right_ :: Int -> Int
right_ i = 2 * i + 2

go_down_ :: STPriorityQueue s e -> Int -> ST s ()
go_down_ q i = do
    let l = left_ i
    let r = right_ i
    let less = (qLess q)
    s <- readSTRef (qSize q)
    if (s <= l)
        then pure ()
        else do
            d <- readSTRef (qData q)
            lVal <- VM.read d l
            let r' = if (s <= r) then l else r
            rVal <- VM.read d r'
            iVal <- VM.read d i
            let (minPos, minVal) = if (lVal `less` rVal) then (l, lVal) else (r', rVal)
            when (minVal `less` iVal) $ do
                VM.write d i minVal
                VM.write d minPos iVal
                go_down_ q minPos

pop_ :: STPriorityQueue s e -> ST s e
pop_ q = do
    s <- readSTRef (qSize q)
    d <- readSTRef (qData q)
    top <- VM.read d 0
    last <- VM.read d (s - 1)
    VM.write d 0 last
    modifySTRef (qSize q) (subtract 1)
    go_down_ q 0
    pure top

pop :: STPriorityQueue s e -> ST s e
pop q = do
    emp <- empty q
    if emp
        then error "Priority queue is empty"
        else pop_ q

go_up_ :: STPriorityQueue s e -> Int -> ST s ()
go_up_ q 0 = pure ()
go_up_ q i = do
    d <- readSTRef (qData q)
    let less = (qLess q)
    let p = parent_ i
    iVal <- VM.read d i
    pVal <- VM.read d p
    when (iVal `less` pVal) $ do
        VM.write d i pVal
        VM.write d p iVal
        go_up_ q p


push_ :: STPriorityQueue s e -> e -> ST s ()
push_ q x = do
    s <- readSTRef (qSize q)
    d <- readSTRef (qData q)
    VM.write d s x
    modifySTRef (qSize q) (+ 1)
    go_up_ q s

grow_ :: STPriorityQueue s e -> ST s ()
grow_ q = do
    oldCap <- capacity q
    d <- readSTRef (qData q)
    d' <- VM.grow d oldCap
    writeSTRef (qData q) d'

push :: STPriorityQueue s e -> e -> ST s ()
push q x = do
    isFull <- full q
    when isFull (grow_ q)
    push_ q x

drain :: STPriorityQueue s e -> ST s [e]
drain q = do
    qEmpty <- empty q
    if qEmpty
        then return []
        else do
            el <- pop q
            els <- drain q
            return (el:els)
