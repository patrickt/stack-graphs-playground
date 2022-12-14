module Bridge where

import Data.Maybe (Maybe)

-- JSON -> "native"

-- "native" -> Fast -> m ()

-- full isomorphism
class Convert extern native where
  convertExtern :: extern -> native
  -- convertExtern = view converted
  convertNative :: native -> extern
  -- convertNarive = review converted

  -- converted :: Iso' extern native
  -- converted = iso convertExtern convertNative

-- partial isomorphism
class Bridge extern native where
  tryBridgeExtern :: extern -> Maybe native
  -- tryBridgeExtern = preview bridged
  bridgeNative :: native -> extern
  -- bridgeNative = review bridged

  -- bridged :: Prism' extern native
  -- bridged = prism' tryBridgedExtern bridgeNative

-- class Injectable extern native where
--   -- causes side effects
--   inject :: MonadEffect m => native -> m (Handle extern)

-- class Projectable extern native where
--   project :: MonadEffect m => Handle extern -> m native
