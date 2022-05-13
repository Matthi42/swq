module Main where

import Prelude
import Concur.Core (Widget)
import Types
import Kontaktsplitter (parseKontakt, toBriefAnrede)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.MUI.DOM as MD
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.MultiAlternative (orr)
import Data.Array ((:), snoc, nub)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (null, trim) as S
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import FFI (storageGet, storageSet)
import Simple.JSON (readJSON, writeJSON)
import Style as Style

-- import Data.Array (many)
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
        $ [ D.div [ P.className "input-edit" ]
              [ inputView model
              , Edit <$> editView model
              , Dialog <$> dialogView model
              ]
          ]
        <> optionals (not $ null model._data.anreden) [ anredenView model._data.anreden ]
    ]

inputView :: Model -> Widget HTML Msg
inputView model =
  D.div [ P.style { display: "flex" } ]
    [ Insert <<< Input
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
    , Insert GoEdit
        <$ MD.button
            [ P.color "primary"
            , P.unsafeMkProp "variant" "contained"
            , P.style { marginLeft: "1rem" }
            , P.disabled $ model.mode == EditMode || (not $ isSuccess model.state)
            , P.onClick
            ]
            [ D.text "✏ Bearbeiten" ]
    , Dialog OpenDialog
        <$ MD.button
            [ P.color "secondary"
            , P.unsafeMkProp "variant" "contained"
            , P.style { marginLeft: "1rem" }
            , P.onClick
            ]
            [ D.text "➕ Titel hinzufügen" ]
    ]

editView :: Model -> Widget HTML EditMsg
editView model = case model.state of
  Success anrede ->
    let
      formValid = not $ S.null anrede.nachname

      disabled = model.mode == InsertMode
    in
      D.div [ P.className "edit-view" ]
        [ ChangeNachname
            <$> MD.textField
                ( [ P.label "Nachname"
                  , P.disabled disabled
                  , P.value anrede.nachname
                  , P.unsafeMkProp "variant" "outlined"
                  , P.unsafeTargetValue <$> P.onChange
                  ]
                    <> optionals (not $ formValid || disabled)
                        [ P.unsafeMkProp "error" "true"
                        , P.unsafeMkProp "helperText" "Nachname muss ausgefüllt werden!"
                        ]
                )
                []
        , ChangeVorname <<< nothingIfEmpty
            <$> MD.textField
                [ P.label "Vorname"
                , P.disabled disabled
                , P.value $ fromMaybe "" anrede.vorname
                , P.unsafeMkProp "variant" "outlined"
                , P.unsafeTargetValue <$> P.onChange
                ]
                []
        , ChangeTitel
            <$> Style.formControl "titel" "Titel"
                ( Style.multiSelect
                    [ P.label "titel"
                    , P.disabled disabled
                    , P.style { width: "250px" }
                    ]
                    anrede.titel
                    (map (\titel -> { t: titel, l: titel }) model._data.titel)
                )
        , ChangeGeschlecht
            <$> Style.formControl "geschlecht" "Geschlecht"
                ( Style.select
                    [ P.label "Geschlecht"
                    , P.disabled disabled
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
                    , P.disabled disabled
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
                , P.disabled $ disabled || not formValid
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
          Failed error -> [ MD.typography 
                          [ P.unsafeMkProp "variant" "h6" ] $ 
                              [ D.pre_ [] $ D.text error ] ]
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
                  , mkCellRAlign $ mayShow $ show <$> anr.geschlecht
                  , mkCellRAlign $ show anr.sprache
                  , mkCell $ toBriefAnrede anr
                  ]
            )
            anreden
    ]
  where
  mkCell cnt = MD.tableCell [] [ D.text cnt ]

  mkCellRAlign cnt = MD.tableCell [ P.unsafeMkProp "align" "right" ] [ D.text cnt ]

dialogView :: Model -> Widget HTML DialogMsg
dialogView model =
  let
    formValid = not $ S.null model.titelInputRaw
  in
    MD.dialog
      [ P.open $ model.mode == DialogMode
      , CloseDialog <$ P.unsafeMkPropHandler "onClose"
      ]
      [ MD.dialogTitle [] [ D.text "Titel hinzufügen" ]
      , MD.dialogContent []
          [ MD.dialogContentText [] [ D.text "Hier können Titel wie \"Dr. med.\" hinzugefügt werden. " ]
          , maybe AddTitel InputTitel
              <$> Style.textFieldWithSubmit model.titelInputRaw "✔ Hinzufügen"
                  ( [ P.label "Titel"
                    , P.placeholder "Dr. med."
                    ]
                      <> optionals (not formValid)
                          [ P.unsafeMkProp "error" "true"
                          , P.unsafeMkProp "helperText" "Titel darf nicht leer sein!"
                          ]
                  )
                  [ P.disabled $ not formValid ]
          , MD.divider [ P.style { margin: "1rem 0" } ] []
          , MD.typography
              [ P.unsafeMkProp "variant" "subtitle2", P.style { marginTop: ".5rem" } ]
              [ D.text "Vorhandene Titel" ]
          , MD.list [ P.unsafeMkProp "dense" "true", P.style { maxHeight: "250px" } ]
              $ map
                  ( \titel ->
                      MD.listItem []
                        [ MD.listItemText [ P.unsafeMkProp "primary" titel ] [] ]
                  )
                  model._data.titel
          ]
      , CloseDialog
          <$ MD.dialogActions []
              [ MD.button [ P.onClick, P.color "primary" ] [ D.text "OK" ]
              ]
      ]

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

update model (Dialog msg) = case msg of
  OpenDialog ->
    model
      { mode = DialogMode
      , prevMode = Just model.mode
      }
  CloseDialog ->
    model
      { mode = fromMaybe InsertMode model.prevMode
      , prevMode = Nothing
      }
  InputTitel newTitel -> model { titelInputRaw = newTitel }
  AddTitel ->
    model
      { _data =
        model._data
          { titel = nub $ snoc model._data.titel $ S.trim model.titelInputRaw
          }
      , titelInputRaw = ""
      }

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

mayShow :: Maybe String -> String
mayShow = fromMaybe "-"

isSuccess :: Result -> Boolean
isSuccess (Success _) = true

isSuccess _ = false

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty "" = Nothing

nothingIfEmpty s = Just s

optionals :: forall m. Monoid m => Boolean -> m -> m
optionals true = identity

optionals false = const mempty
