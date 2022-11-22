module Test.Main where

import Control.Monad.Writer.Trans (execWriterT, lift, tell)
import Prelude
import StackGraph (StackGraph, sampleStackGraph)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.List (List)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List as List
import Effect (Effect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafePartial)
import StringStorage as StringStorage
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

data Prop =
  SymbolCount Int

derive instance eqProp :: Eq Prop
derive instance genProp :: Generic Prop _
instance Show Prop where show = genericShow

basicStackGraphProps :: forall r . StackGraph r -> ST r (List Prop)
basicStackGraphProps sg = execWriterT do
  size <- lift (StringStorage.size sg.symbols)
  tell (List.singleton (SymbolCount size))


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "stack-graphs-playground" do
    describe "import a.b" do
      it "has required nodes" do
        let (props :: List Prop) = ST.run (unsafePartial sampleStackGraph >>= basicStackGraphProps)
        props `shouldContain` SymbolCount 3
  -- sg <- unsafePartial (runST (sampleStackGraph >>= basicStackGraphProps))

  -- log "🍝"
  -- log "You should add some tests."
