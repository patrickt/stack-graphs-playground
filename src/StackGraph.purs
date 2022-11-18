module StackGraph where

import Prelude
import Prim hiding (Symbol)

import Data.Maybe (Maybe (..))
import Control.Monad.ST (ST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Either

import Node (Node, Visibility (..), Nature (..))
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
import File (File)

type StackGraph r = {
  -- TODO: currently using this for everything; we could split it up better
  fresh :: Source r,
  files :: STRef r (HashMap String (Handle File)),
  -- TODO: we may need two maps?
  nodes :: NodeStorage r,
  edges :: NodesToEdges r
}

newStackGraph :: forall r . ST r (StackGraph r)
newStackGraph = do
  fresh <- Source.new
  files <- STRef.new HashMap.empty
  edges <- NodesToEdges.new
  nodes <- NodeStorage.new
  pure { fresh, files, edges, nodes }

makeNodeID :: forall r . Handle File -> StackGraph r -> ST r NodeID
makeNodeID f sg = do
  new <- Source.next sg.fresh
  pure { file: Just f, localIdent: new }

rootNode :: Handle Node
rootNode = Handle.unsafe 1

jumpToNode :: Handle Node
jumpToNode = Handle.unsafe 2

addSymbol :: forall r . String -> StackGraph r -> ST r (Handle Symbol)
addSymbol _ _ = pure (Handle.unsafe 666)

addNode :: forall r . Node -> StackGraph r -> ST r (Maybe (Handle Node))
addNode n s = do
  eRes <- NodeStorage.add s.fresh n s.nodes
  pure (either (const Nothing) Just eRes)

addScopeNode :: forall r . NodeID -> Visibility -> StackGraph r -> ST r (Maybe (Handle Node))
addScopeNode ident visibility = addNode (Node.Scope {ident, visibility})

addPushSymbolNode :: forall r . NodeID -> Handle Symbol -> Nature -> StackGraph r -> ST r (Maybe (Handle Node))
addPushSymbolNode ident symbol nature = addNode (Node.Push { ident, symbol, scoping: Node.Unscoped, nature })

addPopSymbolNode :: forall r . NodeID -> Handle Symbol -> Nature -> StackGraph r -> ST r (Maybe (Handle Node))
addPopSymbolNode ident symbol nature = addNode (Node.Pop { ident, symbol, scoping: Node.Unscoped, nature})

-- addFile :: forall r . String -> StackGraph r -> ST r (Either (Handle File) (Handle File))
-- addFile _ _ = pure (Left (Handle.unsafe 666))

getOrCreateFile :: forall r . String -> StackGraph r -> ST r (Handle File)
getOrCreateFile str sg = do
  allFiles <- STRef.read sg.files
  let mFound = HashMap.lookup str allFiles
  case mFound of
    Just f -> pure f
    Nothing -> do
      new <- Source.nextHandle sg.fresh
      _ <- STRef.modify (HashMap.insert str new) sg.files
      pure new

-- getFile :: forall r . String -> StackGraph r -> ST r (Maybe (Handle File))
-- getFile _ _ = pure Nothing

-- eachNodeInFile :: forall r . Handle File -> StackGraph r -> (Handle Node -> ST r Unit) -> ST r Unit
-- eachNodeInFile _ _ _ = pure unit

-- eachFile :: forall r . StackGraph r -> (Handle File -> ST r Unit) -> ST r Unit
-- eachFile _ _ = pure unit

addEdge :: forall r . Handle Node -> Handle Node -> Int -> StackGraph r -> ST r Unit
addEdge source sink precedence sg = NodesToEdges.add source sink precedence sg.edges

-- This is https://github.github.com/stack-graph-docs/#import
-- aka `import a.b` in Python.
sampleStackGraph :: forall r . (Partial) => ST r (StackGraph r)
sampleStackGraph = do
  sg <- newStackGraph
  file <- getOrCreateFile "__main__" sg
  symA <- addSymbol "a" sg
  nidA1 <- makeNodeID file sg
  mpushA <- addPushSymbolNode nidA1 symA Reference sg
  let Just pushA = mpushA
  addEdge pushA rootNode 0 sg

  symDot <- addSymbol "." sg
  nidDot1 <- makeNodeID file sg
  mPushDot <- addPushSymbolNode nidDot1 symDot Internal sg
  let Just pushDot = mPushDot
  addEdge pushDot pushA 0 sg

  symB <- addSymbol "b" sg
  nidB1 <- makeNodeID file sg
  mPushB <- addPushSymbolNode nidB1 symB Reference sg
  let Just pushB = mPushB
  addEdge pushB pushDot 0 sg

  nidB2 <- makeNodeID file sg
  mPopB <- addPopSymbolNode nidB2 symB Definition sg
  let Just popB = mPopB
  addEdge popB pushB 0 sg

  nidDot2 <- makeNodeID file sg
  mPopDot <- addPopSymbolNode nidDot2 symDot Internal sg
  let Just popDot = mPopDot
  addEdge popDot popB 0 sg

  nidA2 <- makeNodeID file sg
  mPopA <- addPopSymbolNode nidA2 symA Definition sg
  let Just popA = mPopA
  addEdge popA popDot 0 sg

  nidCurr <- makeNodeID file sg
  mCurr <- addScopeNode nidCurr Hidden sg
  let Just curr = mCurr
  addEdge curr popA 0 sg

  pure sg
