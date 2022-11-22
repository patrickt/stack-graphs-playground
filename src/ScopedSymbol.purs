module ScopedSymbol where

import Prim hiding (Symbol)

import Data.List (List)
import Data.Maybe (Maybe)
import Handle (Handle)
import Node (Node)
import Symbol (Symbol)

type ScopedSymbol = {
  symbol :: Handle Symbol,
  scopes :: Maybe (List (Handle Node))
}
