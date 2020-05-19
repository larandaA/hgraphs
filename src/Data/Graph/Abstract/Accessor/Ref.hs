module Data.Graph.Abstract.Accessor.Ref
    ( Ref
    , new, get, set, modify, increment
    ) where

import Data.Graph.Abstract.Accessor (Accessor, liftST)
import Data.STRef (STRef)
import qualified Data.STRef as STRef

type Ref = STRef

{-# INLINE new #-}
new :: a -> Accessor s e v (Ref s a)
new x = liftST (STRef.newSTRef x)

{-# INLINE get #-}
get :: Ref s a -> Accessor s e v a
get r = liftST (STRef.readSTRef r)

{-# INLINE set #-}
set :: Ref s a -> a -> Accessor s e v ()
set r x = liftST (STRef.writeSTRef r x)

{-# INLINE modify #-}
modify :: Ref s a -> (a -> a) -> Accessor s e v ()
modify r f = liftST (STRef.modifySTRef r f)

{-# INLINE increment #-}
increment :: Num a => Ref s a -> Accessor s e v a
increment r = do
    x <- get r
    modify r (+ 1)
    pure x
