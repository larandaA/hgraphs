{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Graph where

import Control.Monad
import Control.Monad.ST
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.STRef

type Vertex = Int

data Edge b = Edge
    { eTo :: Vertex
    , eFrom :: Vertex
    , eVal :: b
    }

eAdj_ :: Edge b -> Adj b
eAdj_ e = Adj
    { aTo = eTo e
    , aVal = eVal e
    }

data Adj b = Adj
    { aTo :: Vertex
    , aVal :: b
    }

data Graph a b = Graph
    { verts :: V.Vector a
    , adjs :: V.Vector (V.Vector (Adj b))
    }


-- build :: (Vertec -> a) -> [Edge b]
-- build :: [(a, [(b, Offset)])]
-- build :: [a] -> [Edge b]

buildFromList :: [a] -> [Edge b] -> Graph a b
buildFromList vs es = Graph
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

data GraphBuilderState a b = GraphBuilderState
    { gbsCount :: Int
    , gbsVerts :: [a]
    , gbsEdges :: [Edge b]
    }

newtype GraphBuilder a b t = GraphBuilder (State (GraphBuilderState a b) t)
    deriving (Functor, Applicative, Monad)

build :: GraphBuilder a b t -> Graph a b
build (GraphBuilder builder) =
    buildFromList (gbsVerts state) (gbsEdges state)
  where
    state = State.execState builder $ GraphBuilderState
        { gbsCount = 0
        , gbsVerts = []
        , gbsEdges = []
        }

vertex :: a -> GraphBuilder a b Vertex
vertex v = GraphBuilder $ do
    state <- State.get
    State.put $ state
        { gbsCount = gbsCount state + 1
        , gbsVerts = v:(gbsVerts state)
        }
    pure (gbsCount state)

edge :: Vertex -> Vertex -> b -> GraphBuilder a b ()
edge v u l = GraphBuilder $ do
    state <- State.get
    if (v >= gbsCount state || u >= gbsCount state) 
    then error "Incorrect edge"
    else State.put $ state
        { gbsEdges = edge:(gbsEdges state)
        }
  where
    edge = Edge
        { eFrom = v
        , eTo = u
        , eVal = l
        }


numVerteces :: Graph a b -> Int
numVerteces = V.length . adjs

numEdges :: Graph a b -> Int
numEdges = V.sum . V.map V.length . adjs

deg :: Int -> Graph a b -> Int
deg v = V.length . (! v) . adjs
