module Data.Queue.Mutable
    ( STQueue
    , newReserve, new
    , empty, full
    , capacity, size
    , pop, push, drain
    ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Mutable as VM
import Data.STRef


data STQueue s e = STQueue
    { qHead :: STRef s Int
    , qTail :: STRef s Int
    , qData :: STRef s (VM.STVector s e)
    }


newReserve :: Int -> ST s (STQueue s e)
newReserve n = do
    d <- VM.new n
    d' <- newSTRef d
    h <- newSTRef 0
    t <- newSTRef 0
    return (STQueue h t d')

new :: ST s (STQueue s e)
new = newReserve 8

empty :: STQueue s e -> ST s Bool
empty q = do
    h <- readSTRef (qHead q)
    t <- readSTRef (qTail q)
    return (h == t)

capacity :: STQueue s e -> ST s Int
capacity q = do
    d <- readSTRef (qData q)
    return (VM.length d)

size :: STQueue s e -> ST s Int
size q = do
    h <- readSTRef (qHead q)
    t <- readSTRef (qTail q)
    c <- capacity q
    if h <= t
        then return (t - h)
        else return (c - h + t)

full :: STQueue s e -> ST s Bool
full q = do
    s <- size q
    c <- capacity q
    return (s + 1 == c)

nextIndex_ :: Int -> Int -> Int
nextIndex_ s i = (i + 1) `mod` s

pop :: STQueue s e -> ST s e
pop q = do
    emp <- empty q
    if emp
        then error "Queue is empty"
        else do
            h <- readSTRef (qHead q)
            d <- readSTRef (qData q)
            c <- capacity q
            modifySTRef (qHead q) (nextIndex_ c)
            VM.read d h

push_ :: STQueue s e -> e -> ST s ()
push_ q x = do
    t <- readSTRef (qTail q)
    d <- readSTRef (qData q)
    c <- capacity q
    VM.write d t x
    modifySTRef (qTail q) (nextIndex_ c)

grow_ :: STQueue s e -> ST s ()
grow_ q = do
    oldCap <- capacity q
    d <- readSTRef (qData q)
    d' <- VM.grow d oldCap
    h <- readSTRef (qHead q)
    t <- readSTRef (qTail q)
    when (t < h) $ do
        forM_ [oldCap - 1, oldCap - 2..h] $ \i -> do
            el <- VM.read d' i
            VM.write d' (i + oldCap) el
        modifySTRef (qHead q) (+oldCap)
        writeSTRef (qData q) d'

push :: STQueue s e -> e -> ST s ()
push q x = do
    isFull <- full q
    when isFull (grow_ q)
    push_ q x

drain :: STQueue s e -> ST s [e]
drain q = do
    qEmpty <- empty q
    if qEmpty
        then return []
        else do
            el <- pop q
            els <- drain q
            return (el:els)
