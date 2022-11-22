module StackGraph where

import Prelude
import Prim hiding (Symbol)

import Data.Maybe (Maybe (..))
import Control.Monad.ST (ST)
import Data.Either (either)

import Node (Node)
import Node as Node
import NodeID (NodeID)
import Handle (Handle)
import Handle as Handle
import Symbol (Symbol)
import Source (Source)
import Source as Source
import NodesToEdges (NodesToEdges)
import NodesToEdges as NodesToEdges
import NodeStorage (NodeStorage)
import NodeStorage as NodeStorage
import StringStorage (StringStorage)
import StringStorage as StringStorage
import File (File)

type StackGraph r = {
  -- TODO: currently using this for everything; we could split it up better
  fresh :: Source r,
  files :: StringStorage File r,
  symbols :: StringStorage Symbol r,
  -- TODO: we may need two maps?
  nodes :: NodeStorage r,
  edges :: NodesToEdges r
}

newStackGraph :: forall r . ST r (StackGraph r)
newStackGraph = do
  fresh <- Source.new
  files <- StringStorage.new
  symbols <- StringStorage.new
  edges <- NodesToEdges.new
  nodes <- NodeStorage.new
  pure { fresh, files, symbols, edges, nodes }

makeNodeID :: forall r . Handle File -> StackGraph r -> ST r NodeID
makeNodeID f sg = do
  new <- Source.next sg.fresh
  pure { file: Just f, localIdent: new }

nodeForID :: forall r . StackGraph r -> NodeID -> ST r (Maybe (Handle Node))
nodeForID _ _ = pure Nothing

get :: forall r . StackGraph r -> Handle Node -> ST r Node
get sg = NodeStorage.get sg.nodes

rootNode :: Handle Node
rootNode = Handle.unsafe 1

jumpToNode :: Handle Node
jumpToNode = Handle.unsafe 2

addSymbol :: forall r . String -> StackGraph r -> ST r (Handle Symbol)
addSymbol str sg = StringStorage.insert sg.fresh str sg.symbols

addNode :: forall r . Node -> StackGraph r -> ST r (Maybe (Handle Node))
addNode n s = do
  eRes <- NodeStorage.add s.fresh n s.nodes
  pure (either (const Nothing) Just eRes)

addScopeNode :: forall r . NodeID -> Boolean -> StackGraph r -> ST r (Maybe (Handle Node))
addScopeNode ident isExported = addNode (Node.Scope {ident, isExported})

addPushSymbolNode :: forall r . NodeID -> Handle Symbol -> Boolean -> StackGraph r -> ST r (Maybe (Handle Node))
addPushSymbolNode ident symbol isReference = addNode (Node.PushSymbol { ident, symbol, isReference })

addPopSymbolNode :: forall r . NodeID -> Handle Symbol -> Boolean -> StackGraph r -> ST r (Maybe (Handle Node))
addPopSymbolNode ident symbol isDefinition = addNode (Node.PopSymbol { ident, symbol, isDefinition })

getOrCreateFile :: forall r . String -> StackGraph r -> ST r (Handle File)
getOrCreateFile str sg = StringStorage.insert sg.fresh str sg.files

addEdge :: forall r . Handle Node -> Handle Node -> Int -> StackGraph r -> ST r Unit
addEdge source sink precedence sg = NodesToEdges.add source sink precedence sg.edges

-- hasEdge :: forall r. Handle Node -> Handle Node -> StackGraph r ->

-- This is https://github.github.com/stack-graph-docs/#import
-- aka `import a.b` in Python.
sampleStackGraph :: forall r . (Partial) => ST r (StackGraph r)
sampleStackGraph = do
  sg <- newStackGraph
  file <- getOrCreateFile "__main__" sg
  symA <- addSymbol "a" sg
  nidA1 <- makeNodeID file sg
  mpushA <- addPushSymbolNode nidA1 symA true sg
  let Just pushA = mpushA
  addEdge pushA rootNode 0 sg

  symDot <- addSymbol "." sg
  nidDot1 <- makeNodeID file sg
  mPushDot <- addPushSymbolNode nidDot1 symDot false sg
  let Just pushDot = mPushDot
  addEdge pushDot pushA 0 sg

  symB <- addSymbol "b" sg
  nidB1 <- makeNodeID file sg
  mPushB <- addPushSymbolNode nidB1 symB true sg
  let Just pushB = mPushB
  addEdge pushB pushDot 0 sg

  nidB2 <- makeNodeID file sg
  mPopB <- addPopSymbolNode nidB2 symB false sg
  let Just popB = mPopB
  addEdge popB pushB 0 sg

  nidDot2 <- makeNodeID file sg
  mPopDot <- addPopSymbolNode nidDot2 symDot true sg
  let Just popDot = mPopDot
  addEdge popDot popB 0 sg

  nidA2 <- makeNodeID file sg
  mPopA <- addPopSymbolNode nidA2 symA false sg
  let Just popA = mPopA
  addEdge popA popDot 0 sg

  nidCurr <- makeNodeID file sg
  mCurr <- addScopeNode nidCurr false sg
  let Just curr = mCurr
  addEdge curr popA 0 sg

  pure sg
