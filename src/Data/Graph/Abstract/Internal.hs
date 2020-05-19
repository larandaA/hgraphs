{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data.Graph.Abstract.Internal
    ( Node, Edge(..), Adj(..)
    , Graph(..), Graph',
    buildFromList
    ) where

import Control.DeepSeq (NFData)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)

type Node = Int

data Edge e = Edge
    { eTo :: Node
    , eFrom :: Node
    , eVal :: e
    }

{-# INLINE eAdj_ #-}
eAdj_ :: Edge e -> Adj e
eAdj_ e = Adj
    { aTo = eTo e
    , aVal = eVal e
    }

data Adj e = Adj
    { aTo :: Node
    , aVal :: e
    } deriving (Generic, NFData)

data Graph e v = Graph
    { verts :: V.Vector v
    , adjs :: V.Vector (V.Vector (Adj e))
    } deriving (Generic, NFData)

type Graph' = Graph ()

buildFromList :: [v] -> [Edge e] -> Graph e v
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
