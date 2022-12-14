module Bridge where

import Data.Lens
import Data.Maybe (Maybe)

-- JSON -> "native"

-- "native" -> Fast -> m ()

-- full isomorphism
class Convert extern native | extern -> native where
  converted :: Iso' extern native

convertExtern :: forall extern native . Convert extern native => extern -> native
convertExtern = view converted

convertNative :: forall extern native . Convert extern native => native -> extern
convertNative = review converted

-- partial isomorphism
class Bridge extern native where
  bridged :: Prism' extern native

tryBridgeExtern :: forall extern native . Bridge extern native => extern -> Maybe native
tryBridgeExtern = preview bridged

  -- tryBridgeExtern = preview bridged
bridgeNative :: forall extern native . Bridge extern native => native -> extern
bridgeNative = review bridged

-- **********
--

-- class Injectable extern native where
--   -- causes side effects
--   inject :: MonadEffect m => native -> m (Handle extern)

-- class Projectable extern native where
--   project :: MonadEffect m => Handle extern -> m native
