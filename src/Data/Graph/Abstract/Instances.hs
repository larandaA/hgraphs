module Data.Graph.Abstract.Instances
    ( flatten
    ) where

import qualified Data.Graph.Abstract as GA
import qualified Data.Graph.Abstract.Builder as GAB
import qualified Data.Graph.Abstract.Internal as GAI
import qualified Data.List as L
import Data.Vector ((!))
import qualified Data.Vector as V

flatten :: GA.Graph e (GA.Graph e v) -> GA.Graph e v
flatten g = GAB.build $ do
    vs <- traverse (traverse GAB.vertex) . V.map GAI.verts . GAI.verts $ g
    V.forM_ (V.zip vs (V.map GAI.adjs (GAI.verts g))) $ \(vsG, adjsG) -> do
        flip V.imapM_ adjsG $ \i adjsV -> do
            let v = vsG ! i
            sequence_ [GAB.edge (GAI.aVal adj) v (vsG ! GAI.aTo adj) | adj <- V.toList adjsV]
    flip V.imapM_ (GAI.adjs g) $ \i adjsV -> do
        V.forM_ adjsV $ \adjV -> do
            sequence_ [GAB.edge (GAI.aVal adjV) v u | v <- V.toList (vs ! i), u <- V.toList (vs ! GAI.aTo adjV)]

instance Functor (GA.Graph e) where
    
    fmap = GA.vmap

instance Applicative (GA.Graph e) where

    pure v = GAB.build $ do
        GAB.vertex v
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

    traverse f g = (\verts' -> g { GAI.verts = verts' }) <$> traverse f (GAI.verts g)
