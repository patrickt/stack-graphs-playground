module Handle (Handle (..)) where

import Prelude

import Data.UInt (UInt)

-- TODO: region parameter here?
newtype Handle (a :: Type) = Handle UInt

derive newtype instance eqHandle :: Eq (Handle a)
derive newtype instance showHandle :: Show (Handle a)
