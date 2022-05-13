module Kontaktsplitter where

import Prelude (bind, flip, pure, show, ($), (<<<), (<>))
import Types --(Anrede, Data, Result(..), Sprache(..), Geschlecht)
import Control.Alt ((<|>))
import Data.Array (foldl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.String (char)

------------------- KONTAKTSPLITTER  ------------------
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
toBriefAnrede a = case a.sprache of
  Englisch -> case a.geschlecht of
    Just M -> "Dear Mr " <> combine a
    Just W -> "Dear Ms " <> combine a
    Nothing -> "To whom it may concern"
  Deutsch -> case a.geschlecht of
    Just M -> "Sehr geehrter Herr " <> combine a
    Just W -> "Sehr geehrte Frau " <> combine a
    Nothing -> "Sehr geehrte Damen und Herren"
  where
    join a = foldl (\x y -> x <> " " <> y) "" a
    combine a = join a.titel <> " " <> a.nachname 
