module Data.Graph.Abstract
    ( Graph, Graph'
    , numVertices
    , vertices, edges
    , vmap, emap, emapc
    , zip, transpose
    , succs, preds
    , degree
    ) where

import Control.Monad
import Data.Graph.Abstract.Internal
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Vector ((!))
import Prelude hiding (zip)

{-# INLINE numVertices #-}
numVertices :: Graph e v -> Int
numVertices = V.length . verts

{-# INLINE vertices #-}
vertices :: Graph e v -> [v]
vertices = V.toList . verts

edges :: Graph e v -> [(v, e, v)]
edges g = V.toList . V.concat . V.toList . V.imap toTuples . adjs $ g
  where
    toTuples i = V.map (\adj -> ((verts g) ! i, aVal adj, (verts g) ! (aTo adj)))

{-# INLINE vmap #-}
vmap :: (v1 -> v2) -> Graph e v1 -> Graph e v2
vmap f g = g { verts = V.map f (verts g) }

{-# INLINE emap #-}
emap :: (e1 -> e2) -> Graph e1 v -> Graph e2 v
emap f g = g { adjs = V.map (V.map (\adj -> adj { aVal = f (aVal adj) })) (adjs g) }

{-# INLINE emapc #-}
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

{-# INLINE degree #-}
degree :: Graph e v -> Graph e Int
degree = vmap length . succs
