module Test.Main where

import Data.Argonaut.Decode.Class
import Prelude
import Test.Spec.Assertions

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.Writer.Trans (execWriterT, lift, tell)
import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either, either, fromLeft, fromRight, isRight)
import Data.Foldable (for_, length)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NE
import Effect (Effect)
import Effect.Aff (launchAff_)
import JSON as JsonSg
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import StackGraph (StackGraph, sampleStackGraph)
import StackGraph as SG
import StringStorage as StringStorage
import Test.QuickCheck (Result, (===))
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (quickCheck)
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

itDeduplicatesSymbols :: List NonEmptyString -> Result
itDeduplicatesSymbols syms = do
  let uniqueCount = length (List.nub syms)
  let result = (ST.run do
                   sg <- SG.newStackGraph
                   for_ syms (\str -> SG.addSymbol (NE.toString str) sg)
                   StringStorage.size sg.symbols)
  result === uniqueCount

newtype PrintJson = PrintJson Json
unprint :: PrintJson -> Json
unprint (PrintJson json) = json

instance Show PrintJson where show (PrintJson p) = Json.stringify p

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    describe "stack-graphs-playground" do
      describe "unit tests" do
        it "parses JSON to JSON type" do
          item <- readTextFile UTF8 "test/sample.json"
          item `shouldNotEqual` ""
          let json = fromRight jsonNull (jsonParser item)
          PrintJson json `shouldNotSatisfy` (unprint >>> Json.isNull)
          (decodeJson json :: _ JsonSg.StackGraph) `shouldSatisfy` isRight

      describe "generative tests" do
        it "deduplicatesSymbols" (quickCheck itDeduplicatesSymbols)

      describe "import a.b" do
        it "has required nodes" do
          let (props :: List Prop) = ST.run (unsafePartial sampleStackGraph >>= basicStackGraphProps)
          props `shouldContain` SymbolCount 3
