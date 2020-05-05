module Data.Graph.Abstract.Accessor.Algorithm.Bfs
    ( bfs, bfsFrom
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor
import Data.Graph.Abstract.Accessor.Queue (Queue)
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import Data.Maybe (isJust)

bfsFrom :: [Vertex s] -> a
        -> (Maybe (a, Edge s) -> Vertex s -> Accessor s e v a)
        -> Accessor s e v (VArray s a)
bfsFrom starts default' f = do
    visited <- varray False
    labels <- varray default'
    queue <- Queue.new

    forM_ starts $ \start -> do
        vset visited start True
        Queue.push queue (Nothing, start)

    bfsIteration queue visited labels f
    pure labels

bfs :: a -> (Maybe (a, Edge s) -> Vertex s -> Accessor s e v a)
    -> Accessor s e v (VArray s a)
bfs default' f = do
    visited <- varray False
    labels <- varray default'
    queue <- Queue.new

    vs <- vertices
    forM_ vs $ \v -> do
        visited' <- vget visited v
        unless visited' $ do
            vset visited v True
            Queue.push queue (Nothing, v)
            bfsIteration queue visited labels f

    pure labels

bfsIteration :: Queue s (Maybe (a, Edge s), Vertex s)
             -> VArray s Bool -> VArray s a
             -> (Maybe (a, Edge s) -> Vertex s -> Accessor s e v a)
             -> Accessor s e v ()
bfsIteration queue visited labels f = do
    whileM_ (not <$> Queue.empty queue) $ do
        (parent, current) <- Queue.pop queue
        label' <- f parent current
        vset labels current label'

        edges' <- outgoing current
        forM_ edges' $ \edge -> do
            successor <- target edge
            visited' <- vget visited successor
            unless visited' $ do
                vset visited successor True
                Queue.push queue (Just (label', edge), successor)
