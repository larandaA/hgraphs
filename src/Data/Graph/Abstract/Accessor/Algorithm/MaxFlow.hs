{-# LANGUAGE TupleSections #-}

module Data.Graph.Abstract.Accessor.Algorithm.MaxFlow
    ( Capacity, Flow, FlowEdge
    , unflow, network, capacities
    , available, push
    , fsource, ftarget
    , edkarp
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor
import Data.Graph.Abstract.Accessor.Queue (Queue)
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import Data.Maybe (isJust)

type Capacity = Int
type Flow = Int
data FlowEdge s = In (Edge s) | Out (Edge s)

unflow :: FlowEdge s -> Edge s
unflow (In edge) = edge
unflow (Out edge) = edge

network :: Accessor s e v (VArray s [FlowEdge s])
network = do
    adj <- varray []
    edges' <- edges
    forM_ edges' $ \edge -> do
        let s = source edge
        sEdges <- vget adj s
        vset adj s ((Out edge):sEdges)
        t <- target edge
        tEdges <- vget adj t
        vset adj t ((In edge):tEdges)
    pure adj

capacities :: (Edge s -> Accessor s e v Capacity)
           -> Accessor s e v (EArray s (Capacity, Flow))
capacities capacity = ebuild (fmap (, 0) . capacity)

available :: EArray s (Capacity, Flow) -> FlowEdge s
          -> Accessor s e v Capacity
available capacities (In edge) = do
    (_, flow) <- eget capacities edge
    pure flow
available capacities (Out edge) = do
    (capacity, flow) <- eget capacities edge
    pure (capacity - flow)

push :: EArray s (Capacity, Flow) -> Flow -> FlowEdge s
     -> Accessor s e v ()
push capacities pushed (In edge) = do
    (capacity, flow) <- eget capacities edge
    eset capacities edge (capacity, flow - pushed)
push capacities pushed (Out edge) = do
    (capacity, flow) <- eget capacities edge
    eset capacities edge (capacity, flow + pushed)

fsource :: FlowEdge s -> Accessor s e v (Vertex s)
fsource (In edge) = target edge
fsource (Out edge) = pure (source edge)

ftarget :: FlowEdge s -> Accessor s e v (Vertex s)
ftarget (In edge) = pure (source edge)
ftarget (Out edge) = target edge

edkarp :: Vertex s -> Vertex s
       -> (Edge s -> Accessor s e v Capacity)
       -> Accessor s e v (EArray s Flow)
edkarp source sink capacity = do
    capacities <- capacities capacity
    network <- network
    edkarpRecursion source sink network capacities
    ebuild (fmap snd . eget capacities)

edkarpRecursion :: Vertex s -> Vertex s
                -> VArray s [FlowEdge s] -> EArray s (Capacity, Flow)
                -> Accessor s e v ()
edkarpRecursion source sink network capacities = do
    paths <- paths source network capacities
    mpath <- vget paths sink
    step mpath
  where
    step Nothing = pure ()
    step (Just []) = pure ()
    step (Just path) = do
        pushable <- traverse (available capacities) path
        let pushed = minimum pushable
        traverse (push capacities pushed) path
        edkarpRecursion source sink network capacities

paths :: Vertex s -> VArray s [FlowEdge s] -> EArray s (Capacity, Flow)
      -> Accessor s e v (VArray s (Maybe [FlowEdge s]))
paths source network capacities = do
    paths <- varray Nothing
    queue <- Queue.new

    vset paths source (Just [])
    Queue.push queue (source, [])

    whileM_ (not <$> Queue.empty queue) $ do
        (current, path) <- Queue.pop queue
        edges' <- vget network current
        forM_ edges' $ \edge -> do
            successor <- ftarget edge
            visited <- isJust <$> vget paths successor
            blocked <- (== 0) <$> available capacities edge
            unless (visited || blocked) $ do
                let path' = edge:path
                vset paths successor (Just path')
                Queue.push queue (successor, path')

    pure paths
