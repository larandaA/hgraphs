{-# LANGUAGE TupleSections #-}

module Data.Graph.Abstract.Example.MaxFlow where

import Data.Graph.Abstract.Instance
import Data.Graph.Abstract.Transform
import Data.Graph.Abstract
import Data.Maybe (isJust)

data Edge = Edge
    { capacity :: Int
    , flow :: Int
    }

available :: Edge -> Int
available e = capacity e - flow e

push :: Int -> Edge -> Edge
push c e = e { flow = flow e + c }

findPath :: (v -> Bool) -> (v -> Bool) -> Graph Edge v -> Graph Edge (Maybe (Int, v), v)
findPath isSource isSink = transformu isSource backtrack (Nothing,)
  where
    backtrack v [] = (Nothing, v)
    backtrack v ((e, (mc, u)):chs) = case mc of
        Nothing | isSink u -> (Just (available e, u), v)
        Nothing | otherwise -> backtrack v chs
        Just (c, _) -> (Just (min c (available e), u), v)

propogatePath :: (v -> Bool) -> Graph Edge (Maybe (Int, v), v) -> Graph Edge (Maybe (Int, v), v)
propogatePath isSource = transformd (isSource . snd) descend id
  where
    descend [] pv = pv
    descend _ (Nothing, v) = (Nothing, v)
    descend (((Nothing, _), _):preds) pv = descend preds pv
    descend (((Just (c, _), _), _):preds) (Just (_, u), v) = (Just (c, u), v)

pushPath :: (Eq v) => Graph Edge (Maybe (Int, v), v) ->  Graph Edge (Maybe (Int, v), v)
pushPath = emapc reflow
  where
    reflow (Nothing, _) e (Nothing, _) = e
    reflow (Just (c, u), v) e (Nothing, v')
        | u == v' = push c e
        | otherwise = e
    reflow (Nothing, v) e (Just (c', u'), v')
        | u' == v = push (-c') e
        | otherwise = e
    reflow (Just (c, u), v) e (Just (c', u'), v')
        | u == v' = push c e
        | u' == v = push (-c') e
        | otherwise = e

reachable :: Graph Edge (Maybe (Int, v), v) -> Bool
reachable = or . vmap (isJust . fst)

buildPath :: (Eq v) => (v -> Bool) -> (v -> Bool) -> Graph Edge v -> Graph Edge (Maybe (Int, v), v)
buildPath isSource isSink = propogatePath isSource . findPath isSource isSink

unmark :: Graph Edge (Maybe (Int, v), v) -> Graph Edge v
unmark = vmap snd

maxFlow :: (Eq v) => (v -> Bool) -> (v -> Bool) -> Graph Edge v -> Graph Edge v
maxFlow isSource isSink g
    | reachable g' = maxFlow isSource isSink . unmark . pushPath $ g'
    | otherwise = unmark g'
  where
    g' = buildPath isSource isSink g
