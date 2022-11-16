module File where

import Prelude

newtype File = File String

derive newtype instance eqFile :: Eq File
instance Show File where show (File s) = s
