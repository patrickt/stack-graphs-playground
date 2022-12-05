module JSON where

import Data.Maybe (Maybe)

data StackGraph = StackGraph {
  edges :: Array Edge,
  files :: Array File,
  nodes :: Array Node
}

type Edge = {
  precedence :: Int,
  source :: NodeID,
  sink :: NodeID
}

type NodeID = {
  file :: Maybe String,
  localID :: Int
}

type File = String

data Node = Node
  { debugInfo :: Maybe (Array DebugInfo)
  , id :: NodeID
  , sourceInfo :: SourceInfo
  , nodeType :: String
  , isReference :: Maybe Boolean
  , symbol :: Maybe String
  , isExported :: Maybe Boolean
  , scope :: Maybe NodeID
  , isDefinition :: Maybe Boolean
}

type DebugInfo =
  { key :: String
  , value :: String
  }

type SourceInfo =
  { span :: Span
  , syntaxType :: Maybe String
  }

type Span =
  { start :: Pos
  , end :: Pos
  }

type Pos =
  { line :: Column
  , column :: Int
  }

type Column =
  { graphemeOffset :: Int
  , utf16Offset :: Int
  , utf8Offset :: Int
  }
