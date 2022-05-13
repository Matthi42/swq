module Test.Main where

import Prelude (Unit, discard, ($))
import Data.Maybe (Maybe(..))
import Data.Array (foldl)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Kontaktsplitter (parseKontakt, toBriefAnrede)
import Types (DialogMsg(..), EditMsg(..), Geschlecht(..), Msg(..), Result(..), Sprache(..), initialModel)
import Main (update, isSuccess)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

parse :: String -> Result
parse = parseKontakt initialModel._data

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do

        describe "User Stories" do

          describe "1" do
            it "zerlegt Eingabe in Bestandtteile" do
              let
                eingabe = "Professor Heinreich Freiherr vom Wald"

                result =
                  { geschlecht: Nothing
                  , sprache: Deutsch
                  , vorname: Just "Heinreich"
                  , titel: [ "Professor" ]
                  , nachname: "Freiherr vom Wald"
                  }
              parse eingabe `shouldEqual` Success result

          describe "2" do
            it "erkennt sowohl <nachname, vornamen> als auch <vornamen nachname>" do
              let
                eingabe = "Dr. Russwurm, Winfried"

                eingabe2 = "Dr. Winfried Russwurm"
              parse eingabe `shouldEqual` parse eingabe2

          describe "3" do
            describe "Englisch" do
              let
                anrede =
                  { geschlecht: Just M
                  , sprache: Englisch
                  , vorname: Just "Peter"
                  , titel: [ "Prof.", "Dr." ]
                  , nachname: "Pan"
                  }
              it "wandelt einen zerlegter Name in die korrekte Briefform um" do
                toBriefAnrede anrede `shouldEqual` "Dear Mr Prof. Dr. Peter Pan"
            describe "Deutsch" do
              let
                anrede =
                  { geschlecht: Just M
                  , sprache: Deutsch
                  , vorname: Just "Peter"
                  , titel: [ "Prof.", "Dr." ]
                  , nachname: "Pan"
                  }
              it "wandelt einen zerlegter Name in die korrekte Briefform um" do
                toBriefAnrede anrede `shouldEqual` "Sehr geehrter Herr Prof. Dr. Peter Pan"

          describe "4" do
            it "erkennt sowohl Kurz- als auch Langversionen von Titeln" do
              isSuccess (parse "Dr. Professor Pan") `shouldEqual` true
              isSuccess (parse "Doktor Prof. Pan") `shouldEqual` true

          describe "5" do
            it "kann das Geschlecht ändern, falls es nicht erkannt wird" do
              let stateBefore = parse "Pan"
                  anredeBefore = { geschlecht: Nothing
                                 , sprache: Deutsch
                                 , vorname: Nothing
                                 , titel: []
                                 , nachname: "Pan"
                                 }
                  modelBefore = initialModel { state = Success anredeBefore }
              stateBefore `shouldEqual` modelBefore.state
              let modelAfter = update modelBefore (Edit $ ChangeGeschlecht $ Just M)
                  anredeAfter = anredeBefore { geschlecht = Just M }
              modelAfter.state `shouldEqual` (Success anredeAfter)

          describe "6" do
            it "erkennt auch manuell hinzugefügte Titel" do
              let cantParseYet = parseKontakt initialModel._data "Dr. med. Pan"
              isSuccess cantParseYet `shouldEqual` false
              let modelAfter = foldl update initialModel 
                   [ Dialog $ InputTitel "Dr. med.", Dialog $ AddTitel ]
              let canParseNow = parseKontakt modelAfter._data "Dr. med. Pan"
                  result = { geschlecht: Nothing
                           , sprache: Deutsch
                           , vorname: Nothing
                           , titel: [ "Dr. med." ]
                           , nachname: "Pan"
                           }
              canParseNow `shouldEqual` (Success result)
