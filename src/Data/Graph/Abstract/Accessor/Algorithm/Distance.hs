module Data.Graph.Abstract.Accessor.Algorithm.Distance
    ( dijkstra, bford
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.PQueue as PQueue
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import qualified Data.Graph.Abstract.Accessor.Ref as Ref
import Data.Maybe (isJust)
import Data.Ord (comparing)

dijkstra :: (Num a, Ord a) => [Vertex s] -> (e -> a) -> Accessor s e v (VArray s (Maybe a))
dijkstra starts cost = do
    distances <- varray Nothing
    queue <- PQueue.newWithComp (comparing snd)

    forM_ starts $ \start -> do
        PQueue.push queue (start, 0)

    whileM_ (not <$> PQueue.empty queue) $ do
        (current, distance) <- PQueue.pop queue
        visited <- isJust <$> (vget distances current)
        unless visited $ do
            vset distances current (Just distance)
            edges' <- outgoing current
            forM_ edges' $ \edge -> do
                successor <- target edge
                label' <- label edge
                let distance' = distance + (cost label')
                PQueue.push queue (successor, distance')

    pure distances

bford :: (Num a, Ord a) => [Vertex s] -> (e -> a)
      -> Accessor s e v (Maybe (VArray s (Maybe a)))
bford starts cost = do
    distances <- varray Nothing
    costs <- ebuild (fmap cost . label)
    let relax = relax' distances costs
    edges <- edges
    n <- length <$> vertices

    forM_ starts $ \start -> vset distances start (Just 0)

    forM_ [0..n - 1] $ \_ -> do
        forM_ edges $ \edge -> do
            relaxed <- relax edge
            when (isJust relaxed) $ do
                target' <- target edge
                vset distances target' relaxed

    negative <- Ref.new False
    forM_ edges $ \edge -> do
        negative' <- isJust <$> relax edge
        when negative' $ Ref.set negative True

    negative <- Ref.get negative
    if negative then (pure Nothing) else (pure (Just distances))
  where
    relax' distances costs edge = do
        cost <- eget costs edge
        sdistance <- vget distances (source edge)
        target' <- target edge
        tdistnace <- vget distances target'
        case (sdistance, tdistnace) of
            (Nothing, _) -> pure Nothing
            (Just sd, Nothing) -> pure (Just (sd + cost))
            (Just sd, Just td) -> if (sd + cost < td)
                                      then pure (Just (sd + cost))
                                      else pure Nothing
