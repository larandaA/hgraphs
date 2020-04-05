{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Abstract.Builder where

import qualified Control.Monad.State as State
import Control.Monad.State (State)
import qualified Data.Graph.Abstract as GA

newtype Vertex s = Vertex { unvertex :: Int }

data GraphBuilderState s e v = GraphBuilderState
    { gbsCount :: Int
    , gbsVerts :: [v]
    , gbsEdges :: [GA.Edge e]
    }

newtype GraphBuilder s e v t = GraphBuilder (State (GraphBuilderState s e v) t)
    deriving (Functor, Applicative, Monad)

build :: (forall s. GraphBuilder s e v t) -> GA.Graph e v
build (GraphBuilder builder) =
    GA.buildFromList_ (reverse . gbsVerts $ state) (gbsEdges state)
  where
    state = State.execState builder $ GraphBuilderState
        { gbsCount = 0
        , gbsVerts = []
        , gbsEdges = []
        }

vertex :: v -> GraphBuilder s e v (Vertex s)
vertex v = GraphBuilder $ do
    state <- State.get
    State.put $ state
        { gbsCount = gbsCount state + 1
        , gbsVerts = v:(gbsVerts state)
        }
    pure (Vertex (gbsCount state))

edge :: e -> Vertex s -> Vertex s -> GraphBuilder s e v ()
edge l (Vertex v) (Vertex u)= GraphBuilder $ do
    state <- State.get
    State.put $ state
        { gbsEdges = edge:(gbsEdges state)
        }
  where
    edge = GA.Edge
        { GA.eFrom = v
        , GA.eTo = u
        , GA.eVal = l
        }

edge' :: Vertex s -> Vertex s -> GraphBuilder s () v ()
edge' = edge ()
