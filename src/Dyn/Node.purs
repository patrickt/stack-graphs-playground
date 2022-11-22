module Dyn.Node where

-- data Nature = Reference | Definition | Internal

-- derive instance eqNature :: Eq Nature
-- derive instance genNature :: Generic Nature _
-- instance Show Nature where show = genericShow

-- data Scoping = Scoped NodeID | Unscoped

-- derive instance eqScoping :: Eq Scoping
-- derive instance genScoping :: Generic Scoping _
-- instance Show Scoping where show = genericShow

-- data Visibility = Exported | Hidden

-- derive instance eqVisibility :: Eq Visibility
-- derive instance genVisibility :: Generic Visibility _
-- instance Show Visibility where show = genericShow

-- type DropScopesNode = { ident :: NodeID }

-- type SymbolNode = { ident :: NodeID, symbol :: Handle Symbol, scoping :: Scoping, nature :: Nature }

-- type ScopeNode = { ident :: NodeID, visibility :: Visibility }

-- data Node
--   = Root
--   | JumpTo
--   | Push SymbolNode
--   | Pop SymbolNode
--   | Scope ScopeNode
--   | Drop DropScopesNode
