module NodeID
       ( NodeID
       , root
       , jumpTo
       ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.UInt (UInt)
import Data.UInt as UInt

import Handle (Handle)
import File (File)

-- TODO: Should this have a region parameter? Probably not.
-- TODO: Would be nice to simulate ControlledOption here.
type NodeID = { file :: Maybe (Handle File), localIdent :: UInt }

showNodeID :: NodeID -> String
showNodeID n = case n.file of
  Nothing -> "<" <> show n.localIdent <> ">"
  Just f  -> "<" <> show n.localIdent <> "(" <> show f <> ")>"

root :: NodeID
root = { file: Nothing, localIdent: UInt.fromInt 1 }

jumpTo :: NodeID
jumpTo = { file:  Nothing, localIdent: UInt.fromInt 2 }
