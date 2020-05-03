module Data.Graph.Abstract.Accessor.Algorithm.Dfs
    ( preorder, preorderFrom
    , postorder, postorderFrom
    ) where

import Control.Monad
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Ref as Ref

preorder :: a -> (Maybe (a, Edge s) -> Vertex s -> Accessor s e v a)
         -> Accessor s e v (VArray s a)
preorder default' f = do
    vs <- vertices
    preorderFrom vs default' f

preorderFrom :: [Vertex s] -> a
             -> (Maybe (a, Edge s) -> Vertex s -> Accessor s e v a)
             -> Accessor s e v (VArray s a)
preorderFrom starts default' f = do
    visited <- varray False
    labels <- varray default'

    forM_ starts $ \v -> do
        vset visited v True
        preorderRecursion Nothing v visited labels f

    pure labels

preorderRecursion :: Maybe (Edge s) -> Vertex s
                  -> VArray s Bool -> VArray s a
                  -> (Maybe (a, Edge s) -> Vertex s -> Accessor s e v a)
                  -> Accessor s e v ()
preorderRecursion mEdge v visited labels f = do
    parentData <- parentLabel mEdge labels
    label' <- f parentData v
    vset labels v label'

    edges' <- outgoing v
    forM_ edges' $ \edge -> do
        successor <- target edge
        visited' <- vget visited successor
        unless visited' $ do
            vset visited successor True
            preorderRecursion (Just edge) successor visited labels f

postorder :: a -> (Vertex s -> [(Edge s, a)] -> [(Edge s, Maybe a)] -> Accessor s e v a)
          -> Accessor s e v (VArray s a)
postorder default' f = do
    vs <- vertices
    postorderFrom vs default' f

postorderFrom :: [Vertex s] -> a
              -> (Vertex s -> [(Edge s, a)] -> [(Edge s, Maybe a)] -> Accessor s e v a)
              -> Accessor s e v (VArray s a)
postorderFrom starts default' f = do
    visited <- varray False
    stacked <- varray False
    labels <- varray default'

    forM_ starts $ \v -> do
        vset visited v True
        postorderRecursion v visited stacked labels f

    pure labels

postorderRecursion :: Vertex s -> VArray s Bool -> VArray s Bool -> VArray s a
                   -> (Vertex s -> [(Edge s, a)] -> [(Edge s, Maybe a)] -> Accessor s e v a)
                   -> Accessor s e v ()
postorderRecursion v visited stacked labels f = do
    vset stacked v True

    edges' <- outgoing v
    childEdges <- Ref.new []
    backEdges <- Ref.new []

    forM_ edges' $ \edge -> do
        successor <- target edge
        visited' <- vget visited successor

        if visited'
            then Ref.modify backEdges (edge:)
            else do
                Ref.modify childEdges (edge:)
                vset visited successor True
                postorderRecursion successor visited stacked labels f

    childEdges' <- Ref.get childEdges
    childData <- childrenLabels childEdges' labels
    backEdges' <- Ref.get backEdges
    backData <- backLabels backEdges' labels stacked
    label' <- f v childData backData
    vset labels v label'

    vset stacked v False

parentLabel :: Maybe (Edge s) -> VArray s a -> Accessor s e v (Maybe (a, Edge s))
parentLabel Nothing _ = pure Nothing
parentLabel (Just e) labels = do
    let v = source e
    vLabel <- vget labels v
    pure (Just (vLabel, e))

backLabels :: [Edge s] -> VArray s a -> VArray s Bool -> Accessor s e v [(Edge s, Maybe a)]
backLabels [] _ _ = pure []
backLabels (e:es) labels stacked = do
    v <- target e
    stacked' <- vget stacked v
    backLabels' <- backLabels es labels stacked
    if stacked'
        then pure ((e, Nothing):backLabels')
        else do
            vLabel <- vget labels v
            pure ((e, Just vLabel):backLabels')

childrenLabels :: [Edge s] -> VArray s a -> Accessor s e v [(Edge s, a)]
childrenLabels [] _ = pure []
childrenLabels (e:es) labels = do
    v <- target e
    vLabel <- vget labels v
    childrenLabels' <- childrenLabels es labels
    pure ((e, vLabel):childrenLabels')
