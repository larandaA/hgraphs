{-# LANGUAGE TupleSections #-}

module Data.Graph.Abstract.Example.MaxFlow where

import Data.Graph.Abstract.Instance
import Data.Graph.Abstract.Transform
import Data.Graph.Abstract
import Data.Maybe (isJust)

available :: (Int, Int) -> Int
available (cap, flow) = cap - flow

findPath :: (v1 -> Bool) -> (v1 -> Bool) -> Graph (Int, Int) v1 -> Graph (Int, Int) (Maybe (Int, v1), v1)
findPath isSource isSink = transformu isSource backtrack (Nothing,)
  where
    backtrack v [] = (Nothing, v)
    backtrack v ((e, (mc, u)):chs) = case mc of
        Nothing | isSink u -> (Just (available e, u), v)
        Nothing | otherwise -> backtrack v chs
        Just (c, _) -> (Just (min c (available e), u), v)

propogate :: (v1 -> Bool) -> Graph (Int, Int) (Maybe (Int, v1), v1) -> Graph (Int, Int) (Maybe (Int, v1), v1)
propogate isSource = transformd (isSource . snd) descend id
  where
    descend [] pv = pv
    descend _ (Nothing, v) = (Nothing, v)
    descend (((Nothing, _), _):preds) pv = descend preds pv
    descend (((Just (c, _), _), _):preds) (Just (_, u), v) = (Just (c, u), v)

push' :: Int -> (Int, Int) -> (Int, Int)
push' c (cap, flow) = (cap, flow + c)

push :: (Eq v1) => Graph (Int, Int) (Maybe (Int, v1), v1) ->  Graph (Int, Int) (Maybe (Int, v1), v1)
push = emapc reflow
  where
    reflow (Nothing, _) e (Nothing, _) = e
    reflow (Just (c, u), v) e (Nothing, v')
        | u == v' = push' c e
        | otherwise = e
    reflow (Nothing, v) e (Just (c', u'), v')
        | u' == v = push' (-c') e
        | otherwise = e
    reflow (Just (c, u), v) e (Just (c', u'), v')
        | u == v' = push' c e
        | u' == v = push' (-c') e
        | otherwise = e

reachable :: Graph (Int, Int) (Maybe (Int, v1), v1) -> Bool
reachable = or . vmap (isJust . fst)

buildPath :: (Eq v1) => (v1 -> Bool) -> (v1 -> Bool) -> Graph (Int, Int) v1 -> Graph (Int, Int) (Maybe (Int, v1), v1)
buildPath isSource isSink = propogate isSource . findPath isSource isSink

unmark :: Graph (Int, Int) (Maybe (Int, v1), v1) -> Graph (Int, Int) v1
unmark = vmap snd

maxFlow :: (Eq v1) => (v1 -> Bool) -> (v1 -> Bool) -> Graph (Int, Int) v1 -> Graph (Int, Int) v1
maxFlow isSource isSink g
    | reachable g' = maxFlow isSource isSink . unmark . push $ g'
    | otherwise = unmark g'
  where
    g' = buildPath isSource isSink g
