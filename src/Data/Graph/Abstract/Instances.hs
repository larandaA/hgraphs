module Data.Graph.Abstract.Instances where

import qualified Data.Graph.Abstract as GA
import qualified Data.List as L

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
    
    gx >>= f = GA.flatten (f <$> gx)

instance Foldable (GA.Graph e) where

    foldr f z = L.foldr f z . GA.vertices

instance Traversable (GA.Graph e) where

    traverse f g = (\verts' -> g { GA.verts = verts' }) <$> traverse f (GA.verts g)
