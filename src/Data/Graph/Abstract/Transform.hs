module Data.Graph.Abstract.Transform
    ( transformu, transformd
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Graph.Abstract.Internal
import qualified Data.Graph.Abstract as GA
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Vector ((!))
import Data.STRef
import qualified Data.Queue.Mutable as QM

bfs_ :: [Node] -> Graph e v -> (V.Vector Node, V.Vector Int, V.Vector Node)
bfs_ vs g = runST $ do
    q <- QM.new
    ordToV <- QM.new
    vToOrd <- VM.replicate n n
    pred <- VM.replicate n n
    inQ <- VM.replicate n False
    curOrd <- newSTRef 0
    mapM_ (QM.push q) vs
    mapM_ (\v -> VM.write inQ v True) vs
    loop q ordToV vToOrd pred curOrd inQ
    ordToVList <- QM.drain ordToV
    vToOrdList <- mapM (VM.read vToOrd) [0..(n - 1)]
    predList <- mapM (VM.read pred) [0..(n - 1)]
    return (V.fromList ordToVList, V.fromList vToOrdList, V.fromList predList)
  where
    n = GA.numVertices g
    loop q ordToV vToOrd pred curOrd inQ = do
        qEmpty <- QM.empty q
        if qEmpty
            then return ()
            else do
                v <- QM.pop q
                QM.push ordToV v
                ord <- readSTRef curOrd
                VM.write vToOrd v ord
                modifySTRef curOrd (+ 1)
                V.forM_ ((adjs g) ! v) $ \adjU -> do
                    uInQ <- VM.read inQ (aTo adjU)
                    when (not uInQ) $ do
                        QM.push q (aTo adjU)
                        VM.write pred (aTo adjU) v
                        VM.write inQ (aTo adjU) True
                loop q ordToV vToOrd pred curOrd inQ

transformu :: (v1 -> Bool) -> (v1 -> [(e, v2)] -> v2) -> (v1 -> v2) -> Graph e v1 -> Graph e v2
transformu isStart f defaultVal g = g { verts = vs }
  where
    n = GA.numVertices g
    startNodes = V.toList. V.map fst . V.filter (isStart . snd) . V.imap (,) . verts $ g
    (ordToV, vToOrd, pred) = bfs_ startNodes g
    vs = V.create $ do
        vsV <- VM.new n
        V.forM_ (V.reverse ordToV) $ \v -> do
            let aChildren = V.toList . V.filter (\adj -> (pred ! (aTo adj)) == v) $ (adjs g ! v)
            bVals <- mapM (VM.read vsV . aTo) aChildren
            let bChildren = L.zip (L.map aVal aChildren) bVals
            VM.write vsV v (f (verts g ! v) bChildren)
        flip V.imapM_ vToOrd $ \v ordV -> when (ordV == n) (VM.write vsV v (defaultVal (verts g ! v)))
        return vsV

transformd :: (v1 -> Bool) -> ([(v2, e)] -> v1 -> v2) -> (v1 -> v2) -> Graph e v1 -> Graph e v2
transformd isStart f defaultVal g = g { verts = vs }
  where
    n = GA.numVertices g
    g' = GA.transpose g
    startNodes = V.toList. V.map fst . V.filter (isStart . snd) . V.imap (,) . verts $ g
    (ordToV, vToOrd, pred) = bfs_ startNodes g
    vs = V.create $ do
        vsV <- VM.new n
        V.forM_ ordToV $ \v -> do
            let aParents = V.toList . V.filter (\adj -> (pred ! v) == (aTo adj)) $ (adjs g' ! v)
            bVals <- mapM (VM.read vsV . aTo) aParents
            let bParents = L.zip bVals (L.map aVal aParents)
            VM.write vsV v (f bParents (verts g ! v))
        flip V.imapM_ vToOrd $ \v ordV -> when (ordV == n) (VM.write vsV v (defaultVal (verts g ! v)))
        return vsV
