module Data.Graph.Abstract.Accessor.Algorithm
    ( distances
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.PQueue as PQueue
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import Data.Maybe (isJust)
import Data.Ord (comparing)

distances :: (Num a, Ord a) => [Vertex s] -> (e -> a) -> Accessor s e v (VArray s (Maybe a))
distances starts cost = do
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
