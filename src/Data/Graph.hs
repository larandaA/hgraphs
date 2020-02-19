{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph where

import Control.Monad
import Control.Monad.ST
import qualified Control.Monad.State as State
import Control.Monad.State (State)
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

degree :: Graph a b -> Graph (a, Int) b
degree g = Graph
    { verts = V.imap (\i val -> (val, V.length ((adjs g) ! i))) (verts g)
    , adjs = adjs g
    }

bfs_ :: [Node] -> Graph a b -> (V.Vector Node, V.Vector Int)
bfs_ vs g = runST $ do
    q <- QM.new
    ordToV <- QM.new
    vToOrd <- VM.replicate n n
    curOrd <- newSTRef 0
    mapM_ (QM.push q) vs
    loop q ordToV vToOrd curOrd
    ordToVList <- QM.drain ordToV
    vToOrdList <- mapM (VM.read vToOrd) [0..(n - 1)]
    return (V.fromList ordToVList, V.fromList vToOrdList)
  where
    n = numVertices g
    loop q ordToV vToOrd curOrd = do
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
                    uOrd <- VM.read vToOrd (aTo adjU)
                    when (uOrd == n) $ QM.push q (aTo adjU)
