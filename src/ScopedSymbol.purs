module ScopedSymbol where

import Prim hiding (Symbol)

import Data.Maybe (Maybe)
import Handle (Handle)
import Node (Node)
import Symbol (Symbol)

type ScopedSymbol = {
  symbol :: Symbol,
  scopes :: Maybe (Array (Handle Node))
}
