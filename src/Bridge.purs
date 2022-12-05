module Bridge where

-- JSON -> "native"

-- "native" -> Fast -> m ()

-- full isomorphism
class Convert foreign native where
  convertForeign :: foreign -> native
  -- convertForeign = view converted
  convertNative :: native -> foreign
  -- convertNarive = review converted

  -- converted :: Iso' foreign native
  -- converted = iso convertForeign convertNative

-- partial isomorphism
class Bridge foreign native where
  tryBridgeForeign :: foreign -> Maybe native
  -- tryBridgeForeign = preview bridged
  bridgeNative :: native -> foreign
  -- bridgeNative = review bridged

  -- bridged :: Prism' foreign native
  -- bridged = prism' tryBridgedForeign bridgeNative

class Injectable foreign native where
  -- causes side effects
  inject :: MonadEffect m => native -> m (Handle foreign)

class Projectable foreign native where
  project :: MonadEffect m => Handle foreign -> m native

data JSONStackGraph = JSONStackGraph
data NativeStackGraph = NativeStackGraph

instance Bridge JSONStackGraph NativeStackGraph

instance Injectable Stateful.StackGraph Native.StackGraph
instance Projectable NativeStackGraph Stateful.StackGraph
