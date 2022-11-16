module StackGraph where

import Prelude
import Prim hiding (Symbol)

import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.HashMap (HashMap)
import Data.HashMap as HashMap

import Node (Node, Visibility (..), Nature (..))
import NodeID (NodeID)
import NodeID as NodeID
import Handle (Handle)
import Handle as Handle
import Symbol (Symbol)
import Source (Source)
import Source as Source
import File (File)

type StackGraph r = {
  -- TODO: currently using this for everything; we could split it up better
  fresh :: Source r,
  files :: STRef r (HashMap String (Handle File))
}

newStackGraph :: forall r . ST r (StackGraph r)
newStackGraph = do
  fresh <- Source.new
  files <- STRef.new HashMap.empty
  pure { fresh, files }

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

-- eachSymbol :: forall r . StackGraph r -> (Handle Symbol -> ST r Unit) -> ST r Unit
-- eachSymbol _ _ = pure unit

-- indexSymbol :: forall r . Handle Symbol -> StackGraph r -> ST r String
-- indexSymbol _ _ = pure ""

addScopeNode :: forall r . NodeID -> Visibility -> StackGraph r -> ST r (Maybe (Handle Node))
addScopeNode _ _ _ = pure Nothing

addPushSymbolNode :: forall r . NodeID -> Handle Symbol -> Nature -> StackGraph r -> ST r (Maybe (Handle Node))
addPushSymbolNode _ _ _ _ = pure Nothing

addPopSymbolNode :: forall r . NodeID -> Handle Symbol -> Nature -> StackGraph r -> ST r (Maybe (Handle Node))
addPopSymbolNode _ _ _ _ = pure Nothing

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

addEdge :: forall r . Handle Node -> Handle Node -> StackGraph r -> ST r Unit
addEdge _ _ _ = pure unit

sampleStackGraph :: forall r . (Partial) => ST r (StackGraph r)
sampleStackGraph = do
  sg <- newStackGraph
  file <- getOrCreateFile "__main__" sg
  symA <- addSymbol "a" sg
  nidA1 <- makeNodeID file sg
  mpushA <- addPushSymbolNode nidA1 symA Reference sg
  let Just pushA = mpushA
  addEdge pushA rootNode sg

  symDot <- addSymbol "." sg
  nidDot1 <- makeNodeID file sg
  mPushDot <- addPushSymbolNode nidDot1 symDot Internal sg
  let Just pushDot = mPushDot
  addEdge pushDot pushA sg

  symB <- addSymbol "b" sg
  nidB1 <- makeNodeID file sg
  mPushB <- addPushSymbolNode nidB1 symB Reference sg
  let Just pushB = mPushB
  addEdge pushB pushDot sg

  nidB2 <- makeNodeID file sg
  mPopB <- addPopSymbolNode nidB2 symB Definition sg
  let Just popB = mPopB
  addEdge popB pushB sg

  nidDot2 <- makeNodeID file sg
  mPopDot <- addPopSymbolNode nidDot2 symDot Internal sg
  let Just popDot = mPopDot
  addEdge popDot popB sg

  nidA2 <- makeNodeID file sg
  mPopA <- addPopSymbolNode nidA2 symA Definition sg
  let Just popA = mPopA
  addEdge popA popDot sg

  nidCurr <- makeNodeID file sg
  mCurr <- addScopeNode nidCurr Hidden sg
  let Just curr = mCurr
  addEdge curr popA sg

  pure sg
