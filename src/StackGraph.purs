module StackGraph where

import Prelude
import Prim hiding (Symbol)

import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Monad.ST (ST)
import Control.Monad.ST as ST

import Node (Node, Visibility, Nature)
import NodeID (NodeID)
import Handle (Handle)
import Handle as Handle
import Symbol (Symbol)
import File (File)

newtype StackGraph (r :: ST.Region) = StackGraph { todo :: String }

addSymbol :: forall r . String -> StackGraph r -> ST r (Handle Symbol)
addSymbol _ _ = pure (Handle.unsafe 666)

eachSymbol :: forall r . StackGraph r -> (Handle Symbol -> ST r Unit) -> ST r Unit
eachSymbol _ _ = pure unit

indexSymbol :: forall r . Handle Symbol -> StackGraph r -> ST r String
indexSymbol _ _ = pure ""

addScopeNode :: forall r . NodeID -> Visibility -> StackGraph r -> ST r (Maybe (Handle Node))
addScopeNode _ _ _ = pure Nothing

addPushSymbolNode :: forall r . NodeID -> Handle String -> Nature -> StackGraph r -> ST r (Maybe (Handle Node))
addPushSymbolNode _ _ _ _ = pure Nothing

addFile :: forall r . String -> StackGraph r -> ST r (Either (Handle File) (Handle File))
addFile _ _ = pure (Left (Handle.unsafe 666))

getOrCreateFile :: forall r . String -> StackGraph r -> ST r (Handle File)
getOrCreateFile _ _ = pure (Handle.unsafe 666)

getFile :: forall r . String -> StackGraph r -> ST r (Maybe (Handle File))
getFile _ _ = pure Nothing

eachNodeInFile :: forall r . Handle File -> StackGraph r -> (Handle Node -> ST r Unit) -> ST r Unit
eachNodeInFile _ _ _ = pure unit

eachFile :: forall r . StackGraph r -> (Handle File -> ST r Unit) -> ST r Unit
eachFile _ _ = pure unit
