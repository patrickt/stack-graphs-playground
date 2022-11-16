module Source where

import Prelude

import Data.UInt (UInt)
import Data.UInt as UInt
import Control.Monad.ST (ST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef

import Handle

type Source r = STRef r UInt

new :: forall r . ST r (Source r)
new = STRef.new (UInt.fromInt 2)

next :: forall r. Source r -> ST r UInt
next = STRef.modify (\a -> a + UInt.fromInt 1)

nextHandle :: forall r a . Source r -> ST r (Handle a)
nextHandle r = do
  n <- next r
  pure (Handle n)
