{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph where

import Control.Monad
import Control.Monad.ST
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import Data.Vector ((!))
import Data.STRef
import qualified Data.Queue.Mutable as QM

newtype Vertex s = Vertex { unvertex :: Int }

type Node = Int

data Edge b = Edge
    { eTo :: Node
    , eFrom :: Node
    , eVal :: b
    }

eAdj_ :: Edge b -> Adj b
eAdj_ e = Adj
    { aTo = eTo e
    , aVal = eVal e
    }

data Adj b = Adj
    { aTo :: Node
    , aVal :: b
    }

data Graph a b = Graph
    { verts :: V.Vector a
    , adjs :: V.Vector (V.Vector (Adj b))
    }

-- build :: (Vertex -> a) -> [Edge b]
-- build :: [(a, [(b, Offset)])]
-- build :: [a] -> [Edge b]

buildFromList_ :: [a] -> [Edge b] -> Graph a b
buildFromList_ vs es = Graph
    { verts = vs' 
    , adjs = V.map V.fromList es'
    }
  where
    n = V.length vs'
    vs' = V.fromList vs
    es' = V.create $ do
        esL <- VM.replicate n []
        forM_ es $ \e -> do
            VM.modify esL ((eAdj_ e):) (eFrom e)
        return esL

data GraphBuilderState s a b = GraphBuilderState
    { gbsCount :: Int
    , gbsVerts :: [a]
    , gbsEdges :: [Edge b]
    }

newtype GraphBuilder s a b t = GraphBuilder (State (GraphBuilderState s a b) t)
    deriving (Functor, Applicative, Monad)

build :: (forall s. GraphBuilder s a b t) -> Graph a b
build (GraphBuilder builder) =
    buildFromList_ (reverse . gbsVerts $ state) (gbsEdges state)
  where
    state = State.execState builder $ GraphBuilderState
        { gbsCount = 0
        , gbsVerts = []
        , gbsEdges = []
        }

vertex :: a -> GraphBuilder s a b (Vertex s)
vertex v = GraphBuilder $ do
    state <- State.get
    State.put $ state
        { gbsCount = gbsCount state + 1
        , gbsVerts = v:(gbsVerts state)
        }
    pure (Vertex (gbsCount state))

edge :: Vertex s -> Vertex s -> b -> GraphBuilder s a b ()
edge (Vertex v) (Vertex u) l = GraphBuilder $ do
    state <- State.get
    State.put $ state
        { gbsEdges = edge:(gbsEdges state)
        }
  where
    edge = Edge
        { eFrom = v
        , eTo = u
        , eVal = l
        }

numVertices :: Graph a b -> Int
numVertices = V.length . verts

vertices :: Graph a b -> [a]
vertices = V.toList . verts

edges :: Graph a b -> [(a, b, a)]
edges g = V.toList . V.concat . V.toList . V.imap toTuples . adjs $ g
  where
    toTuples i = V.map (\adj -> ((verts g) ! i, aVal adj, (verts g) ! (aTo adj)))

vmap :: (a -> b) -> Graph a e -> Graph b e
vmap f g = g { verts = V.map f (verts g) }

emap :: (a -> b) -> Graph v a -> Graph v b
emap f g = g { adjs = V.map (V.map (\adj -> adj { aVal = f (aVal adj) })) (adjs g) }

zip :: Graph a e -> Graph b f -> Graph (a, b) (e, f)
zip g1 g2 = Graph
    { verts = V.zip (verts g1) (verts g2)
    , adjs = V.zipWith zipAdjs (adjs g1) (adjs g2)
    }
  where
    zipAdjs = V.zipWith (\adj1 adj2 -> adj1 { aVal = (aVal adj1, aVal adj2) })

succs :: Graph a b -> Graph [(b, a)] b
succs g = g { verts = V.map (V.toList . V.map (\adj -> (aVal adj, verts g ! (aTo adj)))) (adjs g) }

transpose :: Graph a b -> Graph a b
transpose g = g { adjs =  V.map V.fromList revAdjs }
  where
    revAdjs = V.create $ do
        adjL <- VM.replicate (numVertices g) []
        flip V.imapM_ (adjs g) $ \i vAdjs -> do
            V.forM_ vAdjs $ \adj -> do
                VM.modify adjL ((adj {aTo = i}):) (aTo adj)
        return adjL

preds :: Graph a b -> Graph [(a, b)] b
preds g = g { verts = V.map (V.toList . V.map adjToPred) . adjs . transpose $ g }
  where
    adjToPred adj = (verts g ! (aTo adj), aVal adj)

degree :: Graph a b -> Graph Int b
degree = vmap length . succs

bfs_ :: [Node] -> Graph a b -> (V.Vector Node, V.Vector Int, V.Vector Node)
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

transformu :: (a -> Bool) -> (a -> [(e, b)] -> b) -> (a -> b) -> Graph a e -> Graph b e
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

transformd :: (a -> Bool) -> ([(b, e)] -> a -> b) -> (a -> b) -> Graph a e -> Graph b e
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
