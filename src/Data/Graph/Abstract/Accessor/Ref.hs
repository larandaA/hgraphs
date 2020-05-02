module Data.Graph.Abstract.Accessor.Ref
    ( Ref
    , new, get, set, modify
    ) where

import Data.Graph.Abstract.Accessor (Accessor, liftST)
import Data.STRef (STRef)
import qualified Data.STRef as STRef

type Ref = STRef

new :: a -> Accessor s e v (Ref s a)
new x = liftST (STRef.newSTRef x)

get :: Ref s a -> Accessor s e v a
get r = liftST (STRef.readSTRef r)

set :: Ref s a -> a -> Accessor s e v ()
set r x = liftST (STRef.writeSTRef r x)

modify :: Ref s a -> (a -> a) -> Accessor s e v ()
modify r f = liftST (STRef.modifySTRef r f)
