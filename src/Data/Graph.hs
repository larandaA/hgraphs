module Data.Graph where

import Control.Monad
import Control.Monad.ST
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

{-
data GraphBuilder s a b = GraphBuilder {  }

vertex :: a -> GraphBuilder s a b Vertex
vertex = undefined

edge :: Vertex -> Vertex -> b -> GraphBuilder s a b ()
edge = undefined

build :: GraphBuilder s a b () -> Graph a b
build = undefined
-}

numVerteces :: Graph a b -> Int
numVerteces = V.length . adjs

numEdges :: Graph a b -> Int
numEdges = V.sum . V.map V.length . adjs

deg :: Int -> Graph a b -> Int
deg v = V.length . (! v) . adjs
