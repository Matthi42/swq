module Main where

import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.MUI.DOM as MD
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.MultiAlternative (orr)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import FFI (storageGet, storageSet)
import Foreign (ForeignError(..), readString)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeImpl, writeJSON)
import Style as Style
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.String (char)

-- import Data.Array (many)
------------------- MODEL ----------------
--
data Geschlecht
  = M
  | W

data Sprache
  = Deutsch
  | Englisch

type Titel
  = String

type Anrede
  = { geschlecht :: Maybe Geschlecht
    , titel :: Array Titel
    , sprache :: Sprache
    , vorname :: Maybe String
    , nachname :: String
    }

-- | Diese Daten werden persistiert und beschreiben die Domäne.
type Data
  = { anreden :: Array Anrede
    , titel :: Array String
    }

------------------- STATE  ------------------
--
data Result
  = NothingYet
  | Failed String
  | Success Anrede

data AppMode
  = InsertMode
  | EditMode

type Model
  = { _data :: Data
    , state :: Result
    , inputRaw :: String
    , mode :: AppMode
    }

initialModel :: Model
initialModel =
  { _data:
      { anreden: []
      , titel:
          [ "Dr. rer. nat."
          , "Dr. h.c."
          , "Dr.-Ing"
          , "Dr."
          , "Prof."
          , "Doktor"
          , "Professor"
          ]
      }
  , state: NothingYet
  , inputRaw: ""
  , mode: InsertMode
  }

------------------- MESSAGES  ------------------
data InsertMsg
  = Input String
  | GoEdit

data EditMsg
  = ChangeGeschlecht (Maybe Geschlecht)
  | ChangeVorname (Maybe String)
  | ChangeNachname String
  | ChangeSprache Sprache
  | ChangeTitel (Array Titel)
  | Save

data Msg
  = Insert InsertMsg
  | Edit EditMsg

------------------- VIEW -------------------
-- https://v4.mui.com/components/app-bar/
-- https://github.com/ajnsit/purescript-concur-react-mui/blob/master/examples/src/Calc.purs
--
view :: Model -> Widget HTML Msg
view model =
  orr
    [ D.style [] [ D.text Style.kontaktCSS ]
    , MD.appBar []
        [ MD.toolbar []
            [ MD.container [ P.className "header" ]
                [ MD.typography [ P.unsafeMkProp "variant" "h4" ] [ D.text "Kontaktsplitter" ]
                , MD.button
                    [ P.href "docs.html", P.unsafeMkProp "variant" "contained" ]
                    [ D.text "Dokumentation" ]
                , MD.button
                    [ P.href "tests.html", P.unsafeMkProp "variant" "contained" ]
                    [ D.text "Testergebnisse" ]
                , MD.button
                    [ P.href "https://github.com/Matthi42/swq/blob/master/src/Main.purs", P.unsafeMkProp "variant" "contained" ]
                    [ D.text "Sourcecode" ]
                , MD.button
                    [ P.href "https://github.com/Matthi42/swq/blob/master/test/Main.purs", P.unsafeMkProp "variant" "contained" ]
                    [ D.text "Testcode" ]
                ]
            ]
        ]
    , MD.container [ P.style { marginTop: "100px" }, P.className "content" ]
        [ D.div [ P.className "input-edit" ]
            [ Insert <$> inputView model
            , Edit <$> editView model
            ]
        , anredenView model._data.anreden
        ]
    ]

inputView :: Model -> Widget HTML InsertMsg
inputView model =
  D.div [ P.style { display: "flex" } ]
    [ Input
        <$> MD.textField
            [ P.label "Kontakt hier eingeben"
            , P.value model.inputRaw
            , P.placeholder "Herr Dr. Peter Pan"
            , P.unsafeTargetValue <$> P.onChange
            , P.disabled $ model.mode == EditMode
            , P.style { flexGrow: "1" }
            , P.unsafeMkProp "variant" "outlined"
            ]
            []
    , GoEdit
        <$ MD.button
            [ P.color "primary"
            , P.unsafeMkProp "variant" "contained"
            , P.style { marginLeft: "1rem" }
            , P.disabled $ model.mode == EditMode || (not $ isSuccess model.state)
            , P.onClick
            ]
            [ D.text "✏ Bearbeiten" ]
    ]

editView :: Model -> Widget HTML EditMsg
editView model = case model.state of
  Success anrede ->
    D.div [ P.className "edit-view" ]
      [ ChangeNachname
          <$> MD.textField
              [ P.label "Nachname"
              , P.disabled $ model.mode == InsertMode
              , P.value anrede.nachname
              , P.unsafeMkProp "variant" "outlined"
              , P.unsafeTargetValue <$> P.onChange
              ]
              []
      , ChangeVorname <<< nothingIfEmpty
          <$> MD.textField
              [ P.label "Vorname"
              , P.disabled $ model.mode == InsertMode
              , P.value $ fromMaybe "" anrede.vorname
              , P.unsafeMkProp "variant" "outlined"
              , P.unsafeTargetValue <$> P.onChange
              ]
              []
      , ChangeTitel
          <$> Style.formControl "titel" "Titel"
              ( Style.multiSelect
                  [ P.label "titel"
                  , P.disabled $ model.mode == InsertMode
                  , P.style { width: "250px" }
                  ]
                  anrede.titel
                  (map (\titel -> { t: titel, l: titel }) model._data.titel)
              )
      , ChangeGeschlecht
          <$> Style.formControl "geschlecht" "Geschlecht"
              ( Style.select
                  [ P.label "Geschlecht"
                  , P.disabled $ model.mode == InsertMode
                  , P.style { width: "150px" }
                  ]
                  anrede.geschlecht
                  [ { l: "Keine Angabe", t: Nothing }
                  , { l: "M", t: Just M }
                  , { l: "W", t: Just W }
                  ]
              )
      , ChangeSprache
          <$> Style.formControl "sprache" "Sprache"
              ( Style.select
                  [ P.label "sprache"
                  , P.disabled $ model.mode == InsertMode
                  ]
                  anrede.sprache
                  [ { l: "Deutsch", t: Deutsch }
                  , { l: "Englisch", t: Englisch }
                  ]
              )
      , Save
          <$ MD.button
              [ P.color "primary"
              , P.unsafeMkProp "variant" "contained"
              , P.disabled $ model.mode == InsertMode
              , P.onClick
              ]
              [ D.text "✔ Speichern" ]
      ]
  _ ->
    MD.paper
      [ P.className "msg"
      , P.style
          { backgroundColor:
              case model.state of
                NothingYet -> "lightgray"
                _ -> "red"
          }
      , P.unsafeMkProp "variant" "outlined"
      ]
      $ case model.state of
          Failed error -> [ MD.typography [ P.unsafeMkProp "variant" "h6" ] [ D.text error ] ]
          _ -> [ MD.typography [ P.unsafeMkProp "variant" "h6" ] [ D.text "Bitte Kontakt eingeben!" ] ]

anredenView :: forall a. Array Anrede -> Widget HTML a
anredenView anreden =
  MD.table [ P.className "results", P.unsafeMkProp "size" "small" ]
    [ MD.tableHead []
        [ MD.tableRow [ P.className "result-head" ]
            [ mkCell "Nachname"
            , mkCell "Vorname"
            , mkCellRAlign "Geschlecht"
            , mkCellRAlign "Sprache"
            , mkCell "Briefanrede"
            ]
        ]
    , MD.tableBody []
        $ map
            ( \anr ->
                MD.tableRow []
                  [ mkCell anr.nachname
                  , mkCell $ mayShow anr.vorname
                  , mkCellRAlign $ mayShow anr.geschlecht
                  , mkCellRAlign $ show anr.sprache
                  , mkCell $ toBriefAnrede anr
                  ]
            )
            anreden
    ]
  where
  mkCell cnt = MD.tableCell [] [ D.text cnt ]

  mkCellRAlign cnt = MD.tableCell [ P.unsafeMkProp "align" "right" ] [ D.text cnt ]

------------------- UPDATE --------------------
--
update :: Model -> Msg -> Model
update model (Insert msg) = case msg of
  Input input -> model { state = parseKontakt model._data input, inputRaw = input }
  GoEdit -> model { mode = EditMode }

update model (Edit msg) = case model.state of
  Success anrede -> case msg of
    ChangeGeschlecht x -> model { state = Success $ anrede { geschlecht = x } }
    ChangeVorname x -> model { state = Success $ anrede { vorname = x } }
    ChangeNachname x -> model { state = Success $ anrede { nachname = x } }
    ChangeSprache x -> model { state = Success $ anrede { sprache = x } }
    ChangeTitel x -> model { state = Success $ anrede { titel = x } }
    Save ->
      model
        { mode = InsertMode
        , inputRaw = ""
        , state = NothingYet
        , _data =
          model._data
            { anreden = anrede : model._data.anreden }
        }
  _ -> model

parseKontakt :: Data -> String -> Result
parseKontakt dat = showError <<< flip runParser (pKontakt dat)
  where
  showError (Left err) = Failed $ parseErrorMessage err

  showError (Right suc) = Success suc

pKontakt :: Data -> Parser String Anrede
pKontakt dat = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure
    { geschlecht: Nothing
    , titel: [ "Dr.", "Prof." ]
    , sprache: Deutsch
    , vorname: Nothing
    , nachname: ""
    }

toBriefAnrede :: Anrede -> String
toBriefAnrede = show

------------------- MAIN ---------------------
localStorageKey :: String
localStorageKey = "kontakte"

main :: Effect Unit
main = do
  dataInStorage <- liftEffect $ storageGet localStorageKey
  let
    stored = (eitherToMaybe <<< readJSON) =<< toMaybe dataInStorage

    model = case stored of
      Just stored' -> initialModel { _data = stored' }
      Nothing -> initialModel

    writeToStorage = liftEffect <<< storageSet localStorageKey <<< writeJSON

    go m = do
      msg <- view m
      let
        _ = unsafePerformEffect $ logShow msg
      let
        model' = update m msg
      writeToStorage model'._data
      go model'
  runWidgetInDom "main" $ go model

------------------- HELPER --------------------
--
eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing

eitherToMaybe (Right r) = Just r

mayShow :: forall s. Show s => Maybe s -> String
mayShow (Just s) = show s

mayShow Nothing = "-"

isSuccess :: Result -> Boolean
isSuccess (Success _) = true

isSuccess _ = false

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty "" = Nothing

nothingIfEmpty s = Just s

--------------------------------------------------------------------------------
---------------------------------- INSTANCES -----------------------------------
--------------------------------------------------------------------------------
-- Das sind vom Typsystem generierte Funktionen zum (De-)Serialisieren,
-- Vergleichen und Anzeigen von Datentypen
--
derive instance genericSprache :: Generic Sprache _

derive instance eqSprache :: Eq Sprache

instance showSprache :: Show Sprache where
  show = genericShow

derive instance genericGeschlecht :: Generic Geschlecht _

derive instance eqGeschlecht :: Eq Geschlecht

instance showGeschlecht :: Show Geschlecht where
  show = genericShow

derive instance genericResult :: Generic Result _

instance showResult :: Show Result where
  show = genericShow

derive instance genericAppMode :: Generic AppMode _

derive instance eqAppMode :: Eq AppMode

instance showAppMode :: Show AppMode where
  show = genericShow

derive instance genericMsg :: Generic Msg _

instance showMsg :: Show Msg where
  show = genericShow

derive instance genericIMsg :: Generic InsertMsg _

instance showIMsg :: Show InsertMsg where
  show = genericShow

derive instance genericEMsg :: Generic EditMsg _

instance showEMsg :: Show EditMsg where
  show = genericShow

instance geschlechtReadForeign :: ReadForeign Geschlecht where
  readImpl f =
    readString f
      >>= \s -> case s of
          "M" -> pure M
          "W" -> pure W
          _ -> throwError $ singleton $ ForeignError "Geschlecht konnte nicht gelesen werden."

instance spracheReadForeign :: ReadForeign Sprache where
  readImpl f =
    readString f
      >>= \s -> case s of
          "Deutsch" -> pure Deutsch
          "Englisch" -> pure Englisch
          _ -> throwError $ singleton $ ForeignError "Sprache konnte nicht gelesen werden."

instance spracheWriteForeign :: WriteForeign Sprache where
  writeImpl Deutsch = writeImpl "Deutsch"
  writeImpl Englisch = writeImpl "Englisch"

instance geschlechtWriteForeign :: WriteForeign Geschlecht where
  writeImpl M = writeImpl "M"
  writeImpl W = writeImpl "W"
