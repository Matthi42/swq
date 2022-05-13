module Kontaktsplitter where

import Prelude (bind, flip, pure, show, ($), (<<<))
import Types (Anrede, Data, Result(..), Sprache(..))
import Control.Alt ((<|>))
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
toBriefAnrede = show
