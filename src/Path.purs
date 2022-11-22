module Path where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (lift, runExceptT, throwError)
import Control.Monad.ST (ST)
import Data.Either (Either)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Edge (Edge)
import Handle (Handle)
import Node (Node)
import Node as Node
import NodeID (NodeID)
import ScopedSymbol (ScopedSymbol)
import StackGraph (StackGraph)
import StackGraph as StackGraph

type SymbolStack = List ScopedSymbol
type ScopeStack = List (Handle Node)

type PathEdge = {
  source :: NodeID,
  precedence :: Int
}

type PathEdgeList = List PathEdge

type Path = {
  start :: Handle Node,
  end :: Handle Node,
  symbolStack :: SymbolStack,
  scopeStack :: ScopeStack,
  edges :: PathEdgeList
}

isComplete :: forall r . StackGraph r -> Path -> ST r Boolean
isComplete sg p = do
  n <- StackGraph.get sg p.start
  m <- StackGraph.get sg p.end
  pure (Node.isDefinition n
        && Node.isReference m
        && List.null p.symbolStack)

data PathAppendError
  = IncorrectSourceNode
  | UnknownAttachedScope

maybeM :: forall m a . Applicative m => m a -> Maybe a -> m a
maybeM err = maybe err pure

-- Algorithm 1
append :: forall r . (Partial) => StackGraph r -> Path -> Edge -> ST r (Either PathAppendError Path)
append sg self edge = runExceptT do
  unless (edge.source == self.start) (throwError IncorrectSourceNode)
  sink <- lift (StackGraph.get sg edge.sink)
  case sink of
    -- Push-symbol nodes
    Node.Push {symbol, scoping: Node.Unscoped, nature: _nature} -> do
      let scopedSymbol = { symbol, scopes: Nothing }
      pure self { symbolStack = List.Cons scopedSymbol self.symbolStack }
    Node.Push {symbol, scoping: Node.Scoped scope, nature: _nature} -> do
      sinkScope <- lift (StackGraph.nodeForID sg scope) >>= maybeM (throwError UnknownAttachedScope)
      let scopedSymbol = { symbol, scopes: Just (List.Cons sinkScope self.scopeStack)}
      pure self { symbolStack = List.Cons scopedSymbol self.symbolStack
                }
