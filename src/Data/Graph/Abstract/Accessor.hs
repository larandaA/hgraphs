{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Abstract.Accessor
    ( Accessor
    , liftST, execute
    , Vertex, Edge
    , vertices, value, outgoing, successors, degree
    , edges, label, source, target
    , VArray, varray, vbuild, vget, vset, vgraph, vfold
    , EArray, earray, ebuild, eget, eset, egraph, efold
    , vfind, efind
    ) where

import Control.Monad (join, filterM, forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.Graph.Abstract as GA
import Data.Graph.Abstract (Graph)
import qualified Data.Graph.Abstract.Internal as GAI
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as VM
import Data.Vector.Mutable (STVector)
import Data.Vector ((!))

newtype Accessor s e v a = Accessor (Graph e v -> ST s a)

{-# INLINE unaccessor #-}
unaccessor :: Accessor s e v a -> Graph e v -> ST s a
unaccessor (Accessor af) = af

{-# INLINE unaccessor' #-}
unaccessor' :: (forall s. Accessor s e v a) -> Graph e v -> (forall s. ST s a)
unaccessor' (Accessor af) = af

instance Functor (Accessor s e v) where

    fmap f (Accessor af) = Accessor $ fmap f . af

instance Applicative (Accessor s e v) where

    pure = Accessor . const . pure

    (Accessor aff) <*> (Accessor afa) = Accessor $ \g -> aff g <*> afa g

instance Monad (Accessor s e v) where

    (Accessor afa) >>= f = Accessor $ \g -> afa g >>= (\a -> unaccessor (f a) g)

{-# INLINE liftST #-}
liftST :: ST s a -> Accessor s e v a
liftST = Accessor . const

{-# INLINE execute #-}
execute :: Graph e v -> (forall s. Accessor s e v a) -> a
execute g ac = runST $ unaccessor' ac g

newtype Vertex s = Vertex Int deriving (Eq)

{-# INLINE unvertex #-}
unvertex :: Vertex s -> Int
unvertex (Vertex i) = i

data Edge s = Edge
    { source :: Vertex s
    , offset :: Int
    } deriving (Eq)

vertices :: Accessor s e v [Vertex s]
vertices = Accessor $ \g -> (pure . map Vertex) [0..(GA.numVertices g) - 1]

{-# INLINE value #-}
value :: Vertex s -> Accessor s e v v
value (Vertex i) = Accessor $ \g -> pure (GAI.verts g ! i)

outgoing :: Vertex s -> Accessor s e v [Edge s]
outgoing v@(Vertex i) = Accessor $ \g -> (pure . map (Edge v)) [0..(V.length (GAI.adjs g ! i)) - 1]

edges :: Accessor s e v [Edge s]
edges = vertices >>= fmap concat . sequence . fmap outgoing

{-# INLINE label #-}
label :: Edge s -> Accessor s e v e
label (Edge (Vertex i) j) = Accessor $ \g -> (pure . GAI.aVal) (GAI.adjs g ! i ! j)

{-# INLINE target #-}
target :: Edge s -> Accessor s e v (Vertex s)
target (Edge (Vertex i) j) = Accessor $ \g -> (pure . Vertex . GAI.aTo) (GAI.adjs g ! i ! j)

successors :: Vertex s -> Accessor s e v [Vertex s]
successors v = outgoing v >>= sequence . map target

{-# INLINE degree #-}
degree :: Vertex s -> Accessor s e v Int
degree (Vertex i) = Accessor $ \g -> (pure . V.length) (GAI.adjs g ! i)

newtype VArray s a = VArray (STVector s a)

{-# INLINE unvarray #-}
unvarray :: VArray s a -> STVector s a
unvarray (VArray v) = v

varray :: a -> Accessor s e v (VArray s a)
varray a = Accessor $ \g -> fmap VArray (VM.replicate (GA.numVertices g) a)

vbuild :: (Vertex s -> Accessor s e v a) -> Accessor s e v (VArray s a)
vbuild f = do
    varr <- init
    vs <- vertices
    forM_ vs $ \v -> do
        val <- f v
        vset varr v val
    pure varr
  where
    init = Accessor $ \g -> VArray <$> (VM.new (GA.numVertices g))

{-# INLINE vget #-}
vget :: VArray s a -> Vertex s -> Accessor s e v a
vget (VArray v) (Vertex i) = liftST (VM.read v i)

{-# INLINE vset #-}
vset ::  VArray s a -> Vertex s -> a -> Accessor s e v ()
vset (VArray v) (Vertex i) a = liftST (VM.write v i a)

vgraph :: VArray s a -> Accessor s e v (Graph e a)
vgraph (VArray v) = Accessor $ \g -> fmap (\ls -> g { GAI.verts = ls } ) (V.freeze v)

vfold :: (a -> b -> b) -> b -> VArray s a -> Accessor s e v b
vfold f z varr = do
    vals <- traverse (vget varr) =<< vertices
    pure (foldr f z vals)

vfind :: (v -> Bool) -> Accessor s e v [Vertex s]
vfind f = vertices >>= filterM (fmap f . value)

newtype EArray s a = EArray (Vector (STVector s a))

{-# INLINE unearray #-}
unearray :: EArray s a -> Vector (STVector s a)
unearray (EArray v) = v

earray :: a -> Accessor s e v (EArray s a)
earray a = Accessor $ \g -> (fmap (EArray . V.fromList) . sequence . map replicated . V.toList) (GAI.adjs g)
  where
    replicated v = VM.replicate (V.length v) a

ebuild :: (Edge s -> Accessor s e v a) -> Accessor s e v (EArray s a)
ebuild f = do
    earr <- init
    es <- edges
    forM_ es $ \e -> do
        val <- f e
        eset earr e val
    pure earr
  where
    init' v = VM.new (V.length v)
    init = Accessor $ \g -> (fmap (EArray . V.fromList) . sequence . map init' . V.toList) (GAI.adjs g)

{-# INLINE eget #-}
eget :: EArray s a -> Edge s -> Accessor s e v a
eget (EArray v) (Edge (Vertex i) j) = liftST (VM.read (v ! i) j)

{-# INLINE eset #-}
eset :: EArray s a -> Edge s -> a -> Accessor s e v ()
eset (EArray v) (Edge (Vertex i) j) a = liftST (VM.write (v ! i) j a)

egraph :: EArray s a -> Accessor s e v (Graph a v)
egraph (EArray v) = Accessor $ \g -> do
    v' <- traverse V.freeze v
    pure $ g { GAI.adjs = newAdjs (GAI.adjs g) v' }
  where
    newAdjs = V.zipWith (V.zipWith (\adj a -> adj { GAI.aVal = a }))

efold :: (a -> b -> b) -> b -> EArray s a -> Accessor s e v b
efold f z earr = do
    vals <- traverse (eget earr) =<< edges
    pure (foldr f z vals)

efind :: (e -> Bool) -> Accessor s e v [Edge s]
efind f = edges >>= filterM (fmap f . label)
