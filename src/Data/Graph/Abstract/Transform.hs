{-# LANGUAGE TupleSections #-}

module Data.Graph.Abstract.Transform
    ( transformu', transformu
    , transformd', transformd
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops (whileM_)
import Data.Graph.Abstract (Graph)
import Data.Graph.Abstract.Accessor
import qualified Data.Graph.Abstract.Accessor.Algorithm.Bfs as Bfs
import qualified Data.Graph.Abstract.Accessor.Queue as Queue
import qualified Data.Graph.Abstract.Accessor.Ref as Ref
import qualified Data.Vector.Mutable as MVector

bfs :: (v -> Bool) -> (e -> Bool) -> Accessor s e v (VArray s (Maybe Int, Maybe (Edge s)))
bfs isStart isAllowed = do
    starts <- vfind isStart
    nextIdx <- Ref.new 0
    visited <- varray False
    allowed <- ebuild (fmap isAllowed . label)
    tree <- varray (Nothing, Nothing)
    queue <- Queue.new

    forM_ starts $ \start -> do
        vset visited start True
        Queue.push queue (Nothing, start)

    whileM_ (not <$> Queue.empty queue) $ do
        (parent, current) <- Queue.pop queue
        vset tree current =<< f nextIdx parent

        edges' <- outgoing current
        forM_ edges' $ \edge -> do
            successor <- target edge
            visited' <- vget visited successor
            allowed' <- eget allowed edge
            when (not visited' && allowed') $ do
                vset visited successor True
                Queue.push queue (Just edge, successor)

    pure tree
  where
    f nextIdx me = (, me) <$> fmap Just (Ref.increment nextIdx)

reorder :: VArray s (Maybe Int) -> Accessor s e v [Vertex s]
reorder indices = do
    vs <- vertices
    ms <- vfold mmax Nothing indices
    case ms of
        Nothing -> pure []
        (Just s) -> do
            reordered <- liftST $ MVector.new (s + 1)
            forM_ vs $ \v -> do
                midx <- vget indices v
                case midx of
                    Nothing -> pure ()
                    (Just idx) -> liftST (MVector.write reordered idx v)
            liftST $ traverse (MVector.read reordered) [0..s]
  where
    mmax mx my = (max <$> mx <*> my) <|> mx <|> my

transformu' :: (v1 -> Bool) -> (e -> Bool)
            -> (v1 -> [(e, v2)] -> v2) -> (v1 -> v2)
            -> Graph e v1 -> Graph e v2
transformu' isStart isAllowed f default' g = execute g $ do
    tree <- bfs isStart isAllowed
    values <- vbuild (fmap default' . value)
    vs <- reorder =<< vbuild (fmap fst . vget tree)
    forM_ (reverse vs) $ \v -> do
        value' <- value v
        edges <- filterM (\e -> ((== Just e) . snd) <$> (vget tree =<< target e)) =<< outgoing v
        chs <- traverse (\e -> (,) <$> label e <*> (vget values =<< target e)) edges
        vset values v (f value' chs)
    vgraph values

transformu :: (v1 -> Bool) -> (v1 -> [(e, v2)] -> v2) -> (v1 -> v2) -> Graph e v1 -> Graph e v2
transformu isStart = transformu' isStart (const True)

transformd' :: (v1 -> Bool) -> (e -> Bool)
            -> ([(v2, e)] -> v1 -> v2) -> (v1 -> v2)
            -> Graph e v1 -> Graph e v2
transformd' isStart isAllowed f default' g = execute g $ do
    tree <- bfs isStart isAllowed
    values <- vbuild (fmap default' . value)
    vs <- reorder =<< vbuild (fmap fst . vget tree)
    forM_ vs $ \v -> do
        pred' <- (pred values) =<< vget tree v
        value' <- value v
        vset values v (f pred' value')
    vgraph values
  where
    pred _ (_, Nothing) = pure []
    pred values (_, Just e) = fmap pure $ (,) <$> vget values (source e) <*> label e

transformd :: (v1 -> Bool) -> ([(v2, e)] -> v1 -> v2) -> (v1 -> v2) -> Graph e v1 -> Graph e v2
transformd isStart = transformd' isStart (const True)
