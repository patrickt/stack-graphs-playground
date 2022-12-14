module JSON where

import Data.Argonaut.Decode.Class
import Prelude

import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign.Object as Foreign

data StackGraph = StackGraph {
  edges :: Array Edge,
  files :: Array File,
  nodes :: Array Node
}

derive instance genStackGraph :: Generic StackGraph _
instance Show StackGraph where show = genericShow

lookupM :: forall a . DecodeJson a => String -> Foreign.Object Json -> Either JsonDecodeError (Maybe a)
lookupM k o = case Foreign.lookup k o of
  Nothing -> Right Nothing
  Just v -> Just <$> decodeJson v

lookupE :: forall a . DecodeJson a => String -> Foreign.Object Json -> Either JsonDecodeError a
lookupE k o = case Foreign.lookup k o of
  Nothing -> Left (AtKey k MissingValue)
  Just v -> decodeJson v

instance DecodeJson StackGraph where
  decodeJson = caseJsonObject (Left MissingValue) (\o -> do
     edges <- lookupE "edges" o
     files <- lookupE "files" o
     nodes <- lookupE "nodes" o
     pure (StackGraph{edges, files, nodes}))

type Edge = {
  precedence :: Int,
  source :: NodeID,
  sink :: NodeID
}

type NodeID = {
  file :: Maybe String,
  local_id :: Int
}

type File = String

data Node = Node
  { debug_info :: Maybe (Array DebugInfo)
  , id :: NodeID
  , source_info :: SourceInfo
  , node_type :: String
  , is_reference :: Maybe Boolean
  , symbol :: Maybe String
  , is_exported :: Maybe Boolean
  , scope :: Maybe NodeID
  , is_definition :: Maybe Boolean
}

derive instance genNode :: Generic Node _
instance Show Node where show = genericShow

instance DecodeJson Node where
  decodeJson = caseJsonObject (Left MissingValue) (\o -> do
    debug_info <- lookupM "debug_info" o
    id <- lookupE "id" o
    source_info <- lookupE "source_info" o
    node_type <- lookupE "type" o
    is_reference <- lookupM "is_reference" o
    is_exported <- lookupM "is_exported" o
    is_definition <- lookupM "is_definition" o
    symbol <- lookupM "symbol" o
    scope <- lookupM "scope" o
    pure (Node{debug_info, id, source_info, node_type, is_reference, is_exported, is_definition, symbol, scope}))

type DebugInfo =
  { key :: String
  , value :: String
  }

type SourceInfo =
  { span :: Span
  , syntax_type :: Maybe String
  }

type Span =
  { start :: Pos
  , end :: Pos
  }

type Pos =
  { line :: Int
  , column :: Column
  }

type Column =
  { grapheme_offset :: Int
  , utf16_offset :: Int
  , utf8_offset :: Int
  }
