module Data.Graph.Abstract
    ( Graph, Graph'
    , numVertices
    , vertices, edges
    , vmap, emap, emapc
    , zip, transpose
    , succs, preds
    , degree
    , transformu, transformd
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Graph.Abstract.Internal
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import Data.Vector ((!))
import Data.STRef
import qualified Data.Queue.Mutable as QM
import Prelude hiding (zip)

numVertices :: Graph e v -> Int
numVertices = V.length . verts

vertices :: Graph e v -> [v]
vertices = V.toList . verts

edges :: Graph e v -> [(v, e, v)]
edges g = V.toList . V.concat . V.toList . V.imap toTuples . adjs $ g
  where
    toTuples i = V.map (\adj -> ((verts g) ! i, aVal adj, (verts g) ! (aTo adj)))

vmap :: (v1 -> v2) -> Graph e v1 -> Graph e v2
vmap f g = g { verts = V.map f (verts g) }

emap :: (e1 -> e2) -> Graph e1 v -> Graph e2 v
emap f g = g { adjs = V.map (V.map (\adj -> adj { aVal = f (aVal adj) })) (adjs g) }

emapc :: (v -> e1 -> v -> e2) -> Graph e1 v -> Graph e2 v
emapc f g = g { adjs = V.imap (\v -> V.map (bAdj v)) (adjs g) }
  where
    bAdj v adj = adj { aVal = f (verts g ! v) (aVal adj) (verts g ! (aTo adj)) }

zip :: Graph e1 v1 -> Graph e2 v2 -> Graph (e1, e2) (v1, v2)
zip g1 g2 = Graph
    { verts = V.zip (verts g1) (verts g2)
    , adjs = V.zipWith zipAdjs (adjs g1) (adjs g2)
    }
  where
    zipAdjs = V.zipWith (\adj1 adj2 -> adj1 { aVal = (aVal adj1, aVal adj2) })

succs :: Graph e v -> Graph e [(e, v)]
succs g = g { verts = V.map (V.toList . V.map (\adj -> (aVal adj, verts g ! (aTo adj)))) (adjs g) }

transpose :: Graph e v -> Graph e v
transpose g = g { adjs =  V.map V.fromList revAdjs }
  where
    revAdjs = V.create $ do
        adjL <- VM.replicate (numVertices g) []
        flip V.imapM_ (adjs g) $ \i vAdjs -> do
            V.forM_ vAdjs $ \adj -> do
                VM.modify adjL ((adj {aTo = i}):) (aTo adj)
        return adjL

preds :: Graph e v -> Graph e [(v, e)]
preds g = g { verts = V.map (V.toList . V.map adjToPred) . adjs . transpose $ g }
  where
    adjToPred adj = (verts g ! (aTo adj), aVal adj)

degree :: Graph e v -> Graph e Int
degree = vmap length . succs

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
    n = numVertices g
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
    n = numVertices g
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
    n = numVertices g
    g' = transpose g
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
