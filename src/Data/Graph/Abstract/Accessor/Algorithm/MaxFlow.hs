{-# LANGUAGE TupleSections #-}

module Data.Graph.Abstract.Accessor.Algorithm.MaxFlow
    ( Capacity, Flow, FlowEdge
    , unflow, network, capacities
    , available, push
    , fsource, ftarget
    , edkarp, dinic
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Data.Graph.Abstract.Accessor
import Data.Graph.Abstract.Accessor.Queue (Queue)
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import qualified Data.Graph.Abstract.Accessor.Ref as Ref
import Data.Maybe (isJust, isNothing)

type Capacity = Int
type Flow = Int
data FlowEdge s = In (Edge s) | Out (Edge s)

{-# INLINE unflow #-}
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

{-# INLINE available #-}
available :: EArray s (Capacity, Flow) -> FlowEdge s
          -> Accessor s e v Capacity
available capacities (In edge) = do
    (_, flow) <- eget capacities edge
    pure flow
available capacities (Out edge) = do
    (capacity, flow) <- eget capacities edge
    pure (capacity - flow)

{-# INLINE push #-}
push :: EArray s (Capacity, Flow) -> Flow -> FlowEdge s
     -> Accessor s e v ()
push capacities pushed (In edge) = do
    (capacity, flow) <- eget capacities edge
    eset capacities edge (capacity, flow - pushed)
push capacities pushed (Out edge) = do
    (capacity, flow) <- eget capacities edge
    eset capacities edge (capacity, flow + pushed)

pushThrough :: EArray s (Capacity, Flow) -> [FlowEdge s]
            -> Accessor s e v ()
pushThrough _ [] = pure ()
pushThrough capacities path = do
    pushable <- traverse (available capacities) path
    let pushed = minimum pushable
    traverse (push capacities pushed) path
    pure ()

{-# INLINE fsource #-}
fsource :: FlowEdge s -> Accessor s e v (Vertex s)
fsource (In edge) = target edge
fsource (Out edge) = pure (source edge)

{-# INLINE ftarget #-}
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
        pushThrough capacities path
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

dinic :: Vertex s -> Vertex s
      -> (Edge s -> Accessor s e v Capacity)
      -> Accessor s e v (EArray s Flow)
dinic source sink capacity = do
    capacities <- capacities capacity
    network <- network
    dinicRecursion source sink network capacities
    ebuild (fmap snd . eget capacities)

dinicRecursion :: Vertex s -> Vertex s
               -> VArray s [FlowEdge s] -> EArray s (Capacity, Flow)
               -> Accessor s e v ()
dinicRecursion source sink network capacities = do
    levels <- levels source network capacities
    reachable <- reachable <$> vget levels sink
    when reachable $ do
        layered <- layered network capacities levels
        blockNetwork source sink layered capacities
        dinicRecursion source sink network capacities
  where
    reachable Nothing = False
    reachable (Just d) = d > 0

blockNetwork :: Vertex s -> Vertex s
             -> VArray s [FlowEdge s] -> EArray s (Capacity, Flow)
             -> Accessor s e v ()
blockNetwork source sink network capacities = do
    mpath <- nextPath source
    step mpath
  where
    step Nothing = pure ()
    step (Just path) = do
        pushThrough capacities path
        blockNetwork source sink network capacities

    nextPath current = do
        mpath <- Ref.new Nothing
        when (current == sink) $ Ref.set mpath (Just [])

        whileM_ ((&&) <$> (isNothing <$> Ref.get mpath) <*> ((not . null) <$> vget network current)) $ do
            edges <- vget network current
            let (edge, edges') = (head edges, tail edges)
            blocked <- (== 0) <$> available capacities edge
            if blocked
                then vset network current edges'
                else do
                    successor <- ftarget edge
                    mpath' <- nextPath successor
                    case mpath' of
                        Nothing -> vset network current edges'
                        (Just path') -> Ref.set mpath (Just (edge:path'))
        Ref.get mpath

layered :: VArray s [FlowEdge s] -> EArray s (Capacity, Flow) -> VArray s (Maybe Int)
        -> Accessor s e v (VArray s [FlowEdge s])
layered network capacities levels = do
    vs <- vertices
    vbuild (join . fmap (filterM toNextLevel) . vget network)
  where
    toNextLevel edge = do
        s <- fsource edge
        t <- ftarget edge
        slevel <- vget levels s
        tlevel <- vget levels t
        blocked <- (== 0) <$> available capacities edge
        pure $ (neighbours slevel tlevel) && (not blocked)
    neighbours (Just d) (Just d') = (d + 1 == d')
    neighbours _ _ = False

levels :: Vertex s -> VArray s [FlowEdge s] -> EArray s (Capacity, Flow)
       -> Accessor s e v (VArray s (Maybe Int))
levels source network capacities = do
    levels <- varray Nothing
    queue <- Queue.new

    vset levels source (Just 0)
    Queue.push queue (source, 0)

    whileM_ (not <$> Queue.empty queue) $ do
        (current, level) <- Queue.pop queue
        edges' <- vget network current
        forM_ edges' $ \edge -> do
            successor <- ftarget edge
            visited <- isJust <$> vget levels successor
            blocked <- (== 0) <$> available capacities edge
            unless (visited || blocked) $ do
                let level' = level + 1
                vset levels successor (Just level')
                Queue.push queue (successor, level')

    pure levels
