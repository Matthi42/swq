module Kontaktsplitter where

import Prelude
import Types
import Control.Alt ((<|>))
import Data.Array (fromFoldable, many, some, catMaybes, replicate)
import Data.List (List)
import Data.Foldable (null, intercalate)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (fst, Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, optional, maybe)
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage, fail, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String -- (char)
import Text.Parsing.Parser.Combinators hiding (optional)
import Text.Parsing.Parser.String.Basic -- (char)

------------------- KONTAKTSPLITTER  ------------------
parseKontakt :: Data -> String -> Result
parseKontakt dat input = showError $ runParser input (pKontakt dat <* eof)
  where
  showError (Right suc) = Success suc
  showError (Left err) = 
    let errMsg = parseErrorMessage err
        Position { column } = parseErrorPosition err 
        space = fromCharArray $ replicate (column - 1) ' '
     in Failed $ input <> "\n" <>
         space <> "↑\n" <> 
         "Fehler: " <> errMsg <> " \n" 

pKontakt :: Data -> Parser String Anrede
pKontakt _data = do
  skipSpaces
  { geschlecht, sprache } <-
    maybe
      { geschlecht: Nothing, sprache: Deutsch }
      (\t -> t { geschlecht = Just t.geschlecht })
      <$> optional (pAnrede <* sc1)
  titel <- fromFoldable <$> many (choice (map string _data.titel) <* sc1) -- <?> "Titel"
  Tuple vornamen nachname <- try pVornamenNachname <|> try pNachnameKommaVorname
  skipSpaces
  pure
    { geschlecht
    , sprache
    , titel
    , vorname: 
        if null vornamen 
            then Nothing 
            else Just $ intercalate " " vornamen 
    , nachname
    }

pAnrede :: Parser String { geschlecht :: Geschlecht, sprache :: Sprache }
pAnrede =  (\geschlecht -> { sprache: Deutsch,  geschlecht }) <$> pDeutsch 
       <|> (\geschlecht -> { sprache: Englisch, geschlecht }) <$> pEnglisch
    where 
    pDeutsch = M <$ string "Herr" 
            <|> W <$ string "Frau"
    pEnglisch = W <$ optAbr "Ms"
            <|> W <$ optAbr "Mrs"
            <|> M <$ optAbr "Mr" 

pVornamenNachname :: Parser String (Tuple (List String) String)
pVornamenNachname = Tuple 
    <$> manyTill (pWord <* sc1) (lookAhead $ pNachname <* skipSpaces <* eof)
    <*> pNachname

pNachnameKommaVorname :: Parser String (Tuple (List String) String)
pNachnameKommaVorname = flip Tuple 
    <$> pNachname <* skipSpaces
    <*> (fromMaybe mempty <$> 
            optional ( char ',' 
                     *> manyTill (sc1 *> pWord) (lookAhead $ skipSpaces <* eof)
                     )
        )

pNachname :: Parser String String
pNachname = toString 
         <$> optional (pAdelstitel <* sc1)
         <*> takeWhile1 (letter <|> char '-')
    where toString ad nachname = intercalate " " $ catMaybes [ ad, Just nachname ]
          pAdelstitel = choice $ map string 
            [ "Freiherr von"
            , "Freiherr vom"
            , "von und zum"
            , "von und zu"
            , "von"
            , "zum"
            , "zu"
            , "vom"
            , "of"
            , "de"
            , "van" 
            ]

pWord :: Parser String String 
pWord = takeWhile1 letter

takeWhile1 :: forall a. Parser String a -> Parser String String 
takeWhile1 p = fst <$> match (skipMany p)

-- | Optionally abbreviated string parser
-- | Parses "Mr" as well as "Mr."
optAbr :: String -> Parser String String
optAbr s = string s <* optional (char '.') 

-- | Parses at least one or more whitepace
sc1 :: Parser String Unit
sc1 = void $ space <* skipSpaces

toBriefAnrede :: Anrede -> String
toBriefAnrede a = case a.sprache of
  Englisch -> case a.geschlecht of
    Just M -> "Dear Mr " <> combine
    Just W -> "Dear Ms " <> combine
    Nothing -> "Dear Sir or Madam " <> a.nachname
  Deutsch -> case a.geschlecht of
    Just M -> "Sehr geehrter Herr " <> combine
    Just W -> "Sehr geehrte Frau " <> combine
    Nothing -> "Sehr geehrte Damen und Herren " <> a.nachname
  where combine = intercalate " " a.titel <> " " <> a.nachname 
