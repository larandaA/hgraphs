module Data.Graph.Abstract.Accessor.Algorithm.Mst
    ( kruskal, prim
    ) where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.PQueue as PQueue
import qualified Data.Graph.Abstract.Accessor.Ref as Ref
import qualified Data.Graph.Abstract.Accessor.Dsu as Dsu
import Data.List (sortBy)
import Data.Ord (comparing)

kruskal :: Ord a => (e -> a) -> Accessor s e v [Edge s]
kruskal cost = do
    edges <- edges
    costs <- traverse (fmap cost . label) edges
    let edges' = fmap fst . sortBy (comparing snd) $ (zip edges costs)

    mst <- Ref.new []
    dsu <- Dsu.new
    forM_ edges' $ \edge -> do
        let source' = source edge
        target' <- target edge
        cycle <- (==) <$> Dsu.find dsu source' <*> Dsu.find dsu target'
        when (not cycle) $ do
            Ref.modify mst (edge:)
            Dsu.union dsu source' target'

    Ref.get mst

prim :: Ord a => (e -> a) -> Accessor s e v [Edge s]
prim cost = do
    costs <- ebuild (fmap cost . label)
    visited <- varray False
    vs <- vertices

    mst <- Ref.new []
    forM_ vs $ \v -> do
        visited' <- vget visited v
        when (not visited') $ build v costs visited mst

    Ref.get mst
  where
    build v costs visited mst = do
        vset visited v True

        queue <- PQueue.newWithComp (comparing snd)
        edges <- outgoing v
        forM_ edges $ \edge -> do
            cost <- eget costs edge
            PQueue.push queue (edge, cost)

        whileM_ (not <$> PQueue.empty queue) $ do
            (edge, _) <- PQueue.pop queue
            v <- target edge
            visited' <- vget visited v

            when (not visited') $ do
                Ref.modify mst (edge:)
                vset visited v True
                edges <- outgoing v
                forM_ edges $ \edge -> do
                    cost <- eget costs edge
                    PQueue.push queue (edge, cost)
