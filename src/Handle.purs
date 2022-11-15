module Handle (Handle, unsafe) where

import Prelude

import Data.UInt (UInt)
import Data.UInt as UInt

-- TODO: region parameter here?
newtype Handle (a :: Type) = Handle UInt

derive newtype instance eqHandle :: Eq (Handle a)
derive newtype instance showHandle :: Show (Handle a)

unsafe :: forall a . Int -> Handle a
unsafe i = Handle (UInt.fromInt i)
