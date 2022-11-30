module Path where

import Prelude

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
  | IncorrectPoppedSymbol
  | EmptySymbolStack
  | UnexpectedAttachedScopeList
  | MissingAttachedScopeList

maybeM :: forall m a . Applicative m => m a -> Maybe a -> m a
maybeM err = maybe err pure

-- Algorithm 1
append :: forall r . StackGraph r -> Path -> Edge -> ST r (Either PathAppendError Path)
append sg self edge = runExceptT do
  unless (edge.source == self.start) (throwError IncorrectSourceNode)
  sink <- lift (StackGraph.get sg edge.sink)
  staging <- case sink of
    Node.PushSymbol {symbol} -> do
      let scopedSymbol = { symbol, scopes: Nothing }
      pure self { symbolStack = List.Cons scopedSymbol self.symbolStack }
    Node.PushScopedSymbol {symbol, scope} -> do
      sinkScope <- lift (StackGraph.nodeForID sg scope) >>= maybeM (throwError UnknownAttachedScope)
      let scopedSymbol = { symbol, scopes: Just (List.Cons sinkScope self.scopeStack)}
      pure self { symbolStack = List.Cons scopedSymbol self.symbolStack }
    Node.PopSymbol pop -> do
      {head: top, tail: rest} <- maybeM (throwError EmptySymbolStack) (List.uncons self.symbolStack)
      unless (top.symbol == pop.symbol) (throwError IncorrectPoppedSymbol)
      unless (top.scopes == Nothing) (throwError UnexpectedAttachedScopeList)
      pure self { symbolStack = rest }
    Node.PopScopedSymbol pop -> do
      {head: top} <- maybeM (throwError EmptySymbolStack) (List.uncons self.symbolStack)
      unless (top.symbol == pop.symbol) (throwError IncorrectPoppedSymbol)
      newScopes <- maybeM (throwError MissingAttachedScopeList) top.scopes
      pure self { scopeStack = newScopes }
    Node.Drop _ -> do
      pure self { scopeStack = List.Nil }
    _ -> pure self
  newSource <- lift (StackGraph.get sg edge.source)
  pure staging {
    end = edge.sink,
    edges = List.Cons {source: Node.id newSource, precedence: edge.precedence} (staging.edges)
  }
