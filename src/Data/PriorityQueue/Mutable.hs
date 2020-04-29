{-# LANGUAGE PatternSynonyms #-}

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
import Prelude hiding (last)

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

goDown_ :: STPriorityQueue s e -> Index -> ST s ()
goDown_ q i = do
    let l = left i
    let r = right i
    let less = (qLess q)
    lWithin <- within q l
    when lWithin $ do
        lVal <- get q l
        rWithin <- within q r
        let r' = if rWithin then r else l
        rVal <- get q r'
        iVal <- get q i
        let (min, minVal) = if (lVal `less` rVal) then (l, lVal) else (r', rVal)
        when (minVal `less` iVal) $ do
            set q i minVal
            set q min iVal
            goDown_ q min

pop_ :: STPriorityQueue s e -> ST s e
pop_ q = do
    topVal <- get q Top
    last <- last q
    lastVal <- get q last
    set q Top lastVal
    modifySTRef (qSize q) (subtract 1)
    goDown_ q Top
    pure topVal

pop :: STPriorityQueue s e -> ST s e
pop q = do
    empty <- empty q
    if empty
        then error "Priority queue is empty"
        else pop_ q

goUp_ :: STPriorityQueue s e -> Index -> ST s ()
goUp_ q Top = pure ()
goUp_ q i = do
    let less = (qLess q)
    let p = parent i
    iVal <- get q i
    pVal <- get q p
    when (iVal `less` pVal) $ do
        set q i pVal
        set q p iVal
        goUp_ q p    

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
    i <- append q x
    goUp_ q i

drain :: STPriorityQueue s e -> ST s [e]
drain q = do
    empty <- empty q
    if empty
        then return []
        else do
            el <- pop q
            els <- drain q
            return (el:els)

newtype Index = Index Int

parent :: Index -> Index
parent (Index i) = Index $ (i - 1) `div` 2

left :: Index -> Index
left (Index i) = Index $ 2 * i + 1

right :: Index -> Index
right (Index i) = Index $ 2 * i + 2

get :: STPriorityQueue s e -> Index -> ST s e
get q (Index i) = do
    d <- readSTRef (qData q)
    VM.read d i

set :: STPriorityQueue s e -> Index -> e -> ST s ()
set q (Index i) x = do
    d <- readSTRef (qData q)
    VM.write d i x

append :: STPriorityQueue s e -> e -> ST s Index
append q x = do
    modifySTRef (qSize q) (+ 1)
    i <- last q
    set q i x
    pure i

within :: STPriorityQueue s e -> Index -> ST s Bool
within q (Index i) = do
    s <- readSTRef (qSize q)
    pure (i < s)

last :: STPriorityQueue s e -> ST s Index
last q = do
    s <- readSTRef (qSize q)
    pure $ Index (s - 1)

pattern Top = Index 0
