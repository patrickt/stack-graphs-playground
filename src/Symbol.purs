module Symbol where

import Prelude
import Prim hiding (Symbol)

newtype Symbol = Symbol String

derive newtype instance eqSymbol :: Eq Symbol
instance Show Symbol where show (Symbol s) = s
