module NodeStorage where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Handle (Handle)
import Node (Node)
import NodeID (NodeID)
import Partial.Unsafe (unsafeCrashWith)
import Source (Source)
import Source as Source

type NodeStorage r = STRef r { nodesToHandles :: HashMap Node (Handle Node),
                               handlesToNodes :: HashMap (Handle Node) Node
                             }

new :: forall r. ST r (NodeStorage r)
new = do
  let nodesToHandles = HashMap.empty
      handlesToNodes = HashMap.empty
  STRef.new { nodesToHandles, handlesToNodes }

add :: forall r. Source r -> Node -> NodeStorage r -> ST r (Either (Handle Node) (Handle Node))
add src n s = do
  val <- STRef.read s
  case HashMap.lookup n val.nodesToHandles of
    Just x -> pure (Left x)
    Nothing -> do
      next <- Source.nextHandle src
      void (STRef.modify (\x -> { nodesToHandles: HashMap.insert n next x.nodesToHandles,
                                  handlesToNodes: HashMap.insert next n x.handlesToNodes
                                }) s)
      pure (Right next)

get :: forall r . NodeStorage r -> Handle Node -> ST r Node
get s n = do
  val <- STRef.read s
  case HashMap.lookup n val.handlesToNodes of
    Just x -> pure x
    Nothing -> unsafeCrashWith "BUG: unsafe handle lookup; either I or you have erred wildly"

lookup :: forall r . NodeStorage r -> NodeID -> ST r (Maybe (Handle Node))
lookup _ _ = pure Nothing
