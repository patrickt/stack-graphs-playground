module StringStorage where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe (..))

import Handle (Handle)
import Source (Source)
import Source as Source

type StringStorage a r = STRef r (HashMap String (Handle a))

new :: forall a r . ST r (StringStorage a r)
new = STRef.new HashMap.empty

lookup :: forall a r . String -> StringStorage a r -> ST r (Maybe (Handle a))
lookup str s = do
  all <- STRef.read s
  pure (HashMap.lookup str all)

insert :: forall a r . Source r -> String -> StringStorage a r -> ST r (Handle a)
insert src str s = do
  mVal <- lookup str s
  case mVal of
    Just x -> pure x
    Nothing -> do
      next <- Source.nextHandle src
      next <$ STRef.modify (HashMap.insert str next) s

size :: forall a r . StringStorage a r -> ST r Int
size s = HashMap.size <$> STRef.read s
