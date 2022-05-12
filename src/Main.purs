module Main where

import Prelude
import Concur.Core (Widget)
import Concur.Core.Props (Props)
import Effect.Console (logShow)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import React.DOM.Props (Props) as RP
-- import Concur.React.Widgets (textInputEnter)
-- import Control.Lazy (defer)
import Control.Monad.Error.Class (throwError)
import Control.MultiAlternative (orr)
import Data.Array ((:)) --(catMaybes, cons, intercalate)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List.NonEmpty (singleton)
import Data.List (find)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class (liftEffect)
import FFI (storageGet, storageSet)
import Foreign (ForeignError(..), readString)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON, writeImpl)
import Style as Style
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.String (char)
import Control.Alt ((<|>))
import Concur.React.MUI.DOM as MD

-- import Data.Array (many)
------------------- MODEL ----------------
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
  | Save

data Msg
  = Insert InsertMsg
  | Edit EditMsg

------------------- VIEW -------------------
-- https://v4.mui.com/components/app-bar/
-- https://github.com/ajnsit/purescript-concur-react-mui/blob/master/examples/src/Calc.purs
view :: Model -> Widget HTML Msg
view model =
  orr
    [ D.style [] [ D.text Style.kontaktCSS ]
    , MD.appBar []
        [ MD.toolbar []
            [ MD.container [ P.className "header" ]
                [ MD.typography [ P.unsafeMkProp "variant" "h4" ] [ D.text "Kontaktsplitter" ]
                , MD.button
                    [ P.href "/docs.html", P.unsafeMkProp "variant" "contained" ]
                    [ D.text "Dokumentation" ]
                , MD.button
                    [ P.href "/tests.html", P.unsafeMkProp "variant" "contained" ]
                    [ D.text "Testergebnisse" ]
                ]
            ]
        ]
    , MD.container []
        [ MD.box [ P.style { marginTop: "80px" }, P.className "content" ]
            [ Insert <$> inputView model
            , Edit <$> editView model
            , D.p_ [] $ D.text $ show model.state
            ]
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
            , P.unsafeMkProp "variant" "outlined"
            , P.disabled $ model.mode == EditMode
            , P.style { flexGrow: "1" }
            ]
            []
    , GoEdit
        <$ MD.button
            [ P.color "primary"
            , P.unsafeMkProp "variant" "contained"
            , P.style { marginLeft: "1rem" }
            , P.disabled $ model.mode == EditMode
            , P.onClick
            ]
            [ D.text "✏ Bearbeiten" ]
    ]

editView :: Model -> Widget HTML EditMsg
editView model = case model.state of
  NothingYet ->
    MD.paper
      [ P.className "error-msg"
      , P.style { backgroundColor: "lightgray" }
      , P.unsafeMkProp "variant" "outlined"
      ]
      [ D.p_ [] $ D.text "Bitte Kontakt eingeben!" ]
  Failed error ->
    MD.paper
      [ P.className "error-msg"
      , P.unsafeMkProp "variant" "outlined"
      ]
      [ D.p_ [] $ D.text error ]
  Success anrede ->
    D.div [ P.style { display: "flex" } ]
      [ ChangeGeschlecht
          <$> MD.formControl []
              [ MD.inputLabel [ P.htmlFor "geschlecht" ] [ D.text "Geschlecht" ]
              , select
                  [ P._id "geschlecht"
                  , P.label "Geschlecht"
                  , P.disabled $ model.mode == InsertMode
                  ]
                  anrede.geschlecht
                  [ { l: "Keine Angabe", t: Nothing }
                  , { l: "M", t: Just M }
                  , { l: "W", t: Just W }
                  ]
              ]
      , Save
          <$ MD.button
              [ P.color "primary"
              , P.unsafeMkProp "variant" "contained"
              , P.style { marginLeft: "auto", marginRight: 0 }
              , P.disabled $ model.mode == InsertMode
              , P.onClick
              ]
              [ D.text "✔ Speichern" ]
      ]

-- https://github.com/labordynamicsinstitute/metajelo-ui/blob/master/src/Metajelo/FormUtil.purs
select :: forall a. Eq a => Array (Props RP.Props a) -> a -> Array { l :: String, t :: a } -> Widget HTML a
select props selected opts =
  MD.select
    ([ (unsafeToA <<< P.unsafeTargetValue) <$> P.onChange, P.value findSelected ] <> props)
    $ map (\{ l } -> MD.menuItem [ P.value l ] [ D.text l ]) opts
  where
  unsafeToA a = (unsafePartial $ fromJust $ find (((==) a) <<< _.l) opts).t

  findSelected = (unsafePartial $ fromJust $ find (((==) selected) <<< _.t) opts).l

------------------- UPDATE --------------------
update :: Model -> Msg -> Model
update model (Insert msg) = case msg of
  Input input -> model { state = parseKontakt model._data input, inputRaw = input }
  GoEdit -> model { mode = EditMode }

update model (Edit msg) = case model.state of
  Success anrede -> case msg of
    ChangeGeschlecht ges -> model { state = Success $ anrede { geschlecht = ges } }
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
    , titel: []
    , sprache: Deutsch
    , vorname: Nothing
    , nachname: ""
    }

------------------- MAIN ---------------------
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
eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing

eitherToMaybe (Right r) = Just r

------------------- STORAGE -------------------
localStorageKey :: String
localStorageKey = "kontakte"

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

------------------- INSTANCES ---------------------

derive instance genericSprache :: Generic Sprache _

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

------------------- OLD ------------------- {{{
{-
data Filter = All | Active | Completed
derive instance eqFilter :: Eq Filter
instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

type Todo = {name :: String, done :: Boolean}
type Todos = {filter :: Filter, todos :: Array Todo}
type Model = {todos :: Array Todo}
-- derive instance repGenericTodos :: Rep.Generic Todos _
-- derive instance rfTodos :: ReadForeign Todos
serialiseTodos :: Array Todo -> String
serialiseTodos todosArr = intercalate "\n" (map serialiseTodo todosArr)
  where serialiseTodo {name, done} = name <> "\t" <> if done then "T" else "F"

deserialiseTodos :: String -> Array Todo
deserialiseTodos s = if null s then [] else
  let deserialiseTodo t =
        let prefixLen = countPrefix (_ /= (codePointFromChar '\t')) t
        in { name: take prefixLen t, done: drop (prefixLen+1) t /= "F" }
  in map deserialiseTodo (split (Pattern "\n") s)

localStorageKey :: String
localStorageKey = "todos"

todosWidget :: forall a. Widget HTML a
todosWidget = do
  savedTodosNullable <- liftEffect $ storageGet localStorageKey
  let savedTodos = fromMaybe [] $ map deserialiseTodos $ toMaybe savedTodosNullable
  dyn $ mkTodos {filter: All, todos: savedTodos}

mkTodo :: Array Todo -> Signal HTML (Array Todo)
mkTodo ts = loopW ts \ts' -> D.div' $ pure do
  s <- retryUntil (not <<< null) $ textInputEnter "" true [P.placeholder "What do you want to do?"]
  let newTodos = cons {name: s, done: false} ts'
  pure newTodos

mkTodos :: Todos -> Signal HTML Todos
mkTodos s = loopS s \s' -> do
  ts <- mkTodo s'.todos
  ts' <- map catMaybes (traverse (todo s'.filter) ts)
  fireOnce_ $ liftEffect $ storageSet localStorageKey (serialiseTodos ts')
  filterButtons s' {todos = ts'}

todo :: Filter -> Todo -> Signal HTML (Maybe Todo)
todo p t = if runFilter p t
  then step (Just t) $ D.div'
    [ todo p <<< (\b -> t {done = b}) <$> checkbox t.done
    , do _ <- D.span [mark t.done, P.onDoubleClick] [D.text t.name]
         todo p <<< (\s -> t {name = s}) <$> D.span' [retryUntil (not <<< null) $ textInputEnter t.name false []]
    , defer (\_ -> always Nothing) <$ D.button [P.onClick] [D.text "Delete"]
    ]
  else always (Just t)
  where
    runFilter All _ = true
    runFilter Active t' = not t'.done
    runFilter Completed t' = t'.done
    checkbox b = not b <$ D.input [P._type "checkbox", P.checked b, P.onChange]
    mark done = if done
      then P.style {textDecoration: "line-through"}
      else P.style {}

filterButtons :: Todos -> Signal HTML Todos
filterButtons s = step s $ D.div' (mkFilter <$> filters)
  where
    mkFilter f = UI.button [select f, defer (\_ -> filterButtons (s {filter = f})) <$ P.onClick] [D.text (show f)]
    filters = [All, Active, Completed]
    select f = if s.filter == f
      then P.style {border:"2px solid lightgray"}
      else P.style {}
      }}} -}
