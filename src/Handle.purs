module Handle (Handle (..), unsafe) where

import Prelude

import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Hashable

-- TODO: region parameter here?
newtype Handle (a :: Type) = Handle UInt

derive newtype instance eqHandle :: Eq (Handle a)
derive newtype instance showHandle :: Show (Handle a)

instance hashHandle :: Hashable (Handle a) where
  hash (Handle h) = UInt.toInt h

unsafe :: forall a . Int -> Handle a
unsafe i = Handle (UInt.fromInt i)
