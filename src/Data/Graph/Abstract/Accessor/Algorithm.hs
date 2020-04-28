module Data.Graph.Abstract.Accessor.Algorithm
    ( distances
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import Data.Maybe (isJust)

distances :: [Vertex s] -> Accessor s e v (VArray s (Maybe Int))
distances starts = do
    distances <- varray Nothing
    queue <- Queue.new

    forM_ starts $ \start -> do
        vset distances start (Just 0)
        Queue.push queue (start, 0)

    whileM_ (not <$> Queue.empty queue) $ do
        (current, distance) <- Queue.pop queue
        successors' <- successors current
        forM_ successors' $ \successor -> do
            visited <- isJust <$> (vget distances successor)
            unless visited $ do
                let distance' = distance + 1
                vset distances successor (Just distance')
                Queue.push queue (successor, distance')

    pure distances
