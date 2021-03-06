module Data.Graph.Abstract.Accessor.Algorithm.Dfs
    ( preorder, preorderFrom
    , postorder, postorderFrom
    , dff
    , acyclic, topsort
    , components, connected
    , bicolour, bipartite
    ) where

import Control.Monad
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Ref as Ref
import Data.Maybe (isJust)

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
        visited' <- vget visited v
        unless visited' $ do
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
        visited' <- vget visited v
        unless visited' $ do
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

{-# INLINE parentLabel #-}
parentLabel :: Maybe (Edge s) -> VArray s a -> Accessor s e v (Maybe (a, Edge s))
parentLabel Nothing _ = pure Nothing
parentLabel (Just e) labels = do
    label <- vget labels (source e)
    pure (Just (label, e))

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

dff :: Accessor s e v (VArray s (Maybe (Edge s)))
dff = preorder Nothing f
  where
    f Nothing _ = pure Nothing
    f (Just (_, e)) _ = pure $ Just e

acyclic :: Accessor s e v Bool
acyclic = vfold (&&) True =<< postorder False f
  where
    f v _ es = pure (all (isJust . snd) es)

topsort :: Accessor s e v (Maybe (VArray s Int))
topsort = do
    acyclic' <- acyclic
    if acyclic'
        then do
            nextIdx <- Ref.new 0
            Just <$> postorder 0 (f nextIdx)
        else pure Nothing
  where
    f nextIdx _ _ _ = Ref.increment nextIdx

components :: Accessor s e v (VArray s Int)
components = do
    nextCompId <- Ref.new 0
    preorder 0 (f nextCompId)
  where
    f _ (Just (compId, _)) _ = pure compId
    f nextCompId Nothing _ = Ref.increment nextCompId

connected :: Accessor s e c Bool
connected = do
    comps <- components
    numComps <- vfold count 0 comps
    pure (numComps <= 1)
  where
    count comp num = max (comp + 1) num

bicolour :: Accessor s e v (VArray s Int)
bicolour = preorder 0 f
  where
    f Nothing _ = pure 0
    f (Just (part, _)) _ = pure (1 - part)

bipartite :: Accessor s e v Bool
bipartite = do
    colours <-  bicolour
    vfold (&&) True =<< preorder True (f colours)
  where
    f colours _ v = do
        vcol <- vget colours v
        ncols <- traverse (vget colours) =<< successors v
        pure $ all (/= vcol) ncols
