module NodesToEdges where

import Prelude

import Control.Monad.ST (Region, ST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet

import Handle
import Node
import Edge

-- Poor locality, etc, but will do for now.
type NodesToEdges (r :: Region) = STRef r (HashMap (Handle Node) (HashSet Edge))

new :: forall r . ST r (NodesToEdges r)
new = STRef.new HashMap.empty

add :: forall r . Handle Node -> Handle Node -> Int -> NodesToEdges r -> ST r Unit
add source sink precedence nte = do
  let edge :: Edge
      edge = {source, sink, precedence}
      go :: HashMap (Handle Node) (HashSet Edge) -> HashMap (Handle Node) (HashSet Edge)
      go = HashMap.upsert (HashSet.insert edge) source (HashSet.singleton edge)
  void (STRef.modify go nte)
