module Types where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..), readString)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

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
  | DialogMode

type Model
  = { _data :: Data
    , state :: Result
    , inputRaw :: String
    , mode :: AppMode
    , titelInputRaw :: String
    , prevMode :: Maybe AppMode
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
  , titelInputRaw: ""
  , prevMode: Nothing
  }

------------------- MESSAGES  ------------------
data InsertMsg
  = Input String
  | GoEdit

data DialogMsg
  = AddTitel
  | InputTitel String
  | OpenDialog
  | CloseDialog

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
  | Dialog DialogMsg

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
derive instance eqResult :: Eq Result

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

derive instance genericDMsg :: Generic DialogMsg _

instance showDMsg :: Show DialogMsg where
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
