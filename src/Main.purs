module Main where

import Prelude

import Concur.Core (Widget)
import Effect.Console (logShow)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
-- import Concur.React.Widgets (textInputEnter)
-- import Control.Lazy (defer)
import Control.Monad.Error.Class (throwError)
import Control.MultiAlternative (orr)
-- import Data.Array (catMaybes, cons, intercalate)
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

------------------- MODEL ---------------- {{{

data Geschlecht = M | W
data Sprache = Deutsch | Englisch

derive instance genericSprache :: Generic Sprache _
instance showSprache :: Show Sprache where show = genericShow
derive instance genericGeschlecht :: Generic Geschlecht _
instance showGeschlecht :: Show Geschlecht where show = genericShow

type Titel = String

type Anrede = 
    { geschlecht :: Maybe Geschlecht
    , titel      :: Array Titel
    , sprache    :: Sprache
    , vorname    :: Maybe String 
    , nachname   :: String 
    }

-- | Diese Daten werden persistiert und beschreiben die Domäne.
type Data = 
    { anreden :: Array Anrede
    , titel   :: Array String
    }

data Result 
    = NothingYet
    | Failed String 
    | Success Anrede

derive instance genericResult :: Generic Result _
instance showResult :: Show Result where show = genericShow

type Model = 
    { mData :: Data 
    , mState :: Result
    }

initialModel :: Model 
initialModel = 
    { mData:
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
    , mState: NothingYet
    }

-- }}}

------------------- VIEW ------------------- {{{

-- https://v4.mui.com/components/app-bar/
-- https://github.com/ajnsit/purescript-concur-react-mui/blob/master/examples/src/Calc.purs

view :: Model -> Widget HTML Msg
view model = orr 
      [ D.style [] [ D.text Style.kontaktCSS ]
      , MD.appBar []
        [ MD.toolbar []
          [ MD.container [ P.className "header" ]
            [ MD.typography [ P.unsafeMkProp "variant" "h4" ] [D.text "Kontaktsplitter"]
            , MD.button 
              [ P.href "/docs", P.unsafeMkProp "variant" "contained" ] 
              [ D.text "Dokumentation" ] 
            , MD.button 
              [ P.href "/tests", P.unsafeMkProp "variant" "contained" ] 
              [ D.text "Testergebnisse" ] 
            ]
          ]
        ] 
      , MD.container []
        [ MD.box [ P.style {marginTop: "80px"} ]
          [ Insert <$> inputView model
          , Edit <$> editView model ] 
          ]
      ]

inputView :: Model -> Widget HTML InsertMsg
inputView model = D.div [] 
      [ Input <$> 
          D.input [P._type "text", P.unsafeTargetValue <$> P.onChange]
      , D.text $ show model.mState 
      ]

editView :: Model -> Widget HTML EditMsg
editView model = D.div [] 
    [ ChangeGeschlecht <$> select [{l: "Keine Angabe", t: Nothing}, {l: "M", t: Just M}, {l: "W", t: Just W}]      ]

-- https://github.com/labordynamicsinstitute/metajelo-ui/blob/master/src/Metajelo/FormUtil.purs
select :: forall a. Array {l :: String, t :: a} -> Widget HTML a
select opts = (D.select [ (unsafeToA <<< P.unsafeTargetValue) <$> P.onChange] $ 
        map (\{l} -> D.option [] [ D.text l ]) opts)
    where unsafeToA :: String -> a
          unsafeToA a = (unsafePartial $ fromJust $ find (((==) a) <<< _.l) opts).t

-- }}}

------------------- MESSAGES / UPDATE ------------------ {{{

data InsertMsg = Input String
data EditMsg = ChangeGeschlecht (Maybe Geschlecht)

data Msg = Insert InsertMsg
         | Edit EditMsg

derive instance genericMsg :: Generic Msg _
instance showMsg :: Show Msg where show = genericShow

derive instance genericIMsg :: Generic InsertMsg _
instance showIMsg :: Show InsertMsg where show = genericShow

derive instance genericEMsg :: Generic EditMsg _
instance showEMsg :: Show EditMsg where show = genericShow

------------------- UPDATE --------------------

update :: Model -> Msg -> Model
update model (Insert msg) = case msg of 
    Input input -> model { mState = parseKontakt model.mData input }
update model (Edit msg) = case model.mState of 
    Success anrede -> 
        case msg of 
             ChangeGeschlecht ges -> 
                 model { mState = Success $ anrede { geschlecht = ges } }
    _ -> model

parseKontakt :: Data -> String -> Result
parseKontakt dat = showError <<< flip runParser (pKontakt dat)
    where showError (Left err) = Failed $ parseErrorMessage err
          showError (Right suc) = Success suc

pKontakt :: Data -> Parser String Anrede
pKontakt dat = do 
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure { geschlecht: Nothing 
       , titel: []
       , sprache: Deutsch
       , vorname: Nothing
       , nachname: ""
       }

-- }}}

------------------- MAIN --------------------- {{{

main :: Effect Unit
main = do 
  dataInStorage <- liftEffect $ storageGet localStorageKey
  let stored = (eitherToMaybe <<< readJSON) =<< toMaybe dataInStorage
      model = case stored of
            Just stored' -> initialModel { mData = stored' }
            Nothing -> initialModel
      writeToStorage = liftEffect <<< storageSet localStorageKey <<< writeJSON
      go m = do
         msg <- view m
         let _ = unsafePerformEffect $ logShow msg
         let model' = update m msg 
         writeToStorage model'.mData 
         go model'
  runWidgetInDom "main" $ go model

-- }}}

------------------- HELPER -------------------- {{{

eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right r) = Just r

-- }}}

------------------- STORAGE ------------------- {{{
                  
localStorageKey :: String
localStorageKey = "kontakte"

instance geschlechtReadForeign :: ReadForeign Geschlecht where
  readImpl f = readString f >>= \s -> case s of
    "M" -> pure M 
    "W" -> pure W
    _ -> throwError $ singleton $ ForeignError "Geschlecht konnte nicht gelesen werden."

instance spracheReadForeign :: ReadForeign Sprache where
  readImpl f = readString f >>= \s -> case s of
    "Deutsch" -> pure Deutsch 
    "Englisch" -> pure Englisch
    _ -> throwError $ singleton $ ForeignError "Sprache konnte nicht gelesen werden."

instance spracheWriteForeign :: WriteForeign Sprache where
  writeImpl Deutsch = writeImpl "Deutsch" 
  writeImpl Englisch = writeImpl "Englisch" 

instance geschlechtWriteForeign :: WriteForeign Geschlecht where
  writeImpl M = writeImpl "M" 
  writeImpl W = writeImpl "W" 

-- }}}

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
