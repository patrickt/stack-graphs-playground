module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import NodeID as NodeID

main :: Effect Unit
main = do
  log (show NodeID.root)
