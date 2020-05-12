module Data.Graph.Abstract.Accessor.Dsu
    ( Dsu
    , new, find, union
    ) where

import Control.Monad
import Data.Graph.Abstract.Accessor

data Dsu s = Dsu
    { size :: VArray s Int
    , parent :: VArray s (Vertex s)
    }

new :: Accessor s e v (Dsu s)
new = do
    s <- varray 1
    p <- vbuild pure
    pure $ Dsu { size = s, parent = p }

find :: Dsu s -> Vertex s -> Accessor s e v (Vertex s)
find dsu v = do
    p <- vget (parent dsu) v
    when (p /= v) $ do
        p' <- find dsu p
        vset (parent dsu) v p'
    vget (parent dsu) v

union' :: Dsu s -> Vertex s -> Vertex s -> Accessor s e v ()
union' dsu r r' = do
    when (r /= r') $ do
        sr <- vget (size dsu) r
        sr' <- vget (size dsu) r'
        vset (parent dsu) r' r
        vset (size dsu) r (sr + sr')

union :: Dsu s -> Vertex s -> Vertex s -> Accessor s e v ()
union dsu v u = do
    rv <- find dsu v
    ru <- find dsu u
    sv <- vget (size dsu) rv
    su <- vget (size dsu) ru
    if (sv < su)
        then union' dsu ru rv
        else union' dsu rv ru
