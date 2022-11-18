module NodeStorage where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Control.Monad.ST (ST)
import Data.Either
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Maybe

import Source (Source)
import Source as Source
import Node (Node)
import Handle (Handle)

type NodeStorage r = STRef r (HashMap Node (Handle Node))

new :: forall r. ST r (NodeStorage r)
new = STRef.new (HashMap.empty)

add :: forall r. Source r -> Node -> NodeStorage r -> ST r (Either (Handle Node) (Handle Node))
add src n s = do
  val <- STRef.read s
  case HashMap.lookup n val of
    Just x -> pure (Left x)
    Nothing -> do
      next <- Source.nextHandle src
      void (STRef.modify (HashMap.insert n next) s)
      pure (Right next)
