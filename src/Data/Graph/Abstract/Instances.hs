module Data.Graph.Abstract.Instances where

import qualified Data.Graph.Abstract as GA
import qualified Data.List as L
import Data.Vector ((!))
import qualified Data.Vector as V

flatten :: GA.Graph e (GA.Graph e v) -> GA.Graph e v
flatten g = GA.build $ do
    vs <- traverse (traverse GA.vertex) . V.map GA.verts . GA.verts $ g
    V.forM_ (V.zip vs (V.map GA.adjs (GA.verts g))) $ \(vsG, adjsG) -> do
        flip V.imapM_ adjsG $ \i adjsV -> do
            let v = vsG ! i
            sequence_ [GA.edge (GA.aVal adj) v (vsG ! GA.aTo adj) | adj <- V.toList adjsV]
    flip V.imapM_ (GA.adjs g) $ \i adjsV -> do
        V.forM_ adjsV $ \adjV -> do
            sequence_ [GA.edge (GA.aVal adjV) v u | v <- V.toList (vs ! i), u <- V.toList (vs ! GA.aTo adjV)]

instance Functor (GA.Graph e) where
    
    fmap = GA.vmap

instance Applicative (GA.Graph e) where

    pure v = GA.build $ do
        GA.vertex v
        pure ()

    gf <*> gx = do
        f <- gf
        x <- gx
        pure (f x)

instance Monad (GA.Graph e) where
    
    gx >>= f = flatten (f <$> gx)

instance Foldable (GA.Graph e) where

    foldr f z = L.foldr f z . GA.vertices

instance Traversable (GA.Graph e) where

    traverse f g = (\verts' -> g { GA.verts = verts' }) <$> traverse f (GA.verts g)
