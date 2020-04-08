{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Abstract.Builder
    ( Vertex, Builder
    , build, vertex
    , edge, edge'
    ) where

import qualified Control.Monad.State as State
import Control.Monad.State (State)
import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Internal as GAI

newtype Vertex s = Vertex { unvertex :: Int }

data BuilderState s e v = BuilderState
    { gbsCount :: Int
    , gbsVerts :: [v]
    , gbsEdges :: [GAI.Edge e]
    }

newtype Builder s e v t = Builder (State (BuilderState s e v) t)
    deriving (Functor, Applicative, Monad)

build :: (forall s. Builder s e v t) -> GA.Graph e v
build (Builder builder) =
    GAI.buildFromList (reverse . gbsVerts $ state) (gbsEdges state)
  where
    state = State.execState builder $ BuilderState
        { gbsCount = 0
        , gbsVerts = []
        , gbsEdges = []
        }

vertex :: v -> Builder s e v (Vertex s)
vertex v = Builder $ do
    state <- State.get
    State.put $ state
        { gbsCount = gbsCount state + 1
        , gbsVerts = v:(gbsVerts state)
        }
    pure (Vertex (gbsCount state))

edge :: e -> Vertex s -> Vertex s -> Builder s e v ()
edge l (Vertex v) (Vertex u) = Builder $ do
    state <- State.get
    State.put $ state
        { gbsEdges = edge:(gbsEdges state)
        }
  where
    edge = GAI.Edge
        { GAI.eFrom = v
        , GAI.eTo = u
        , GAI.eVal = l
        }

edge' :: Vertex s -> Vertex s -> Builder s () v ()
edge' = edge ()
