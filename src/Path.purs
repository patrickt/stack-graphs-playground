module Path where

import Prelude

import Control.Monad.ST (ST)
import Data.Array as Array

import Handle (Handle)
import Node (Node)
import Node as Node
import NodeID (NodeID)
import ScopedSymbol (ScopedSymbol)
import StackGraph (StackGraph)
import StackGraph as StackGraph

type SymbolStack = Array ScopedSymbol
type ScopeStack = Array (Handle Node)

type PathEdge = {
  source :: NodeID,
  precedence :: Int
}

type PathEdgeList = Array PathEdge

type Path = {
  startNode :: Handle Node,
  endNode :: Handle Node,
  symbolStack :: SymbolStack,
  scopeStack :: ScopeStack,
  edges :: PathEdgeList
}

isComplete :: forall r . StackGraph r -> Path -> ST r Boolean
isComplete sg p = do
  n <- StackGraph.get sg p.startNode
  m <- StackGraph.get sg p.endNode
  pure (Node.isDefinition n
        && Node.isReference m
        && Array.null p.symbolStack)
