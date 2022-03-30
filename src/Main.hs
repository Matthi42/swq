{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Data.Text                 (stripPrefix, pack)
import           Lucid
import           Network.HTTP.Types        (status200, status404)
import           Network.URI.Encode        (decodeText)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, Response, rawPathInfo,
                                            responseFile, responseLBS,
                                            strictRequestBody)
import           Network.Wai.Handler.Warp  (run)
import           Prelude                   hiding (ByteString, length)
import           Types
import Data.Char (isNumber)
import           Text.Megaparsec      hiding (chunk)
import           Text.Megaparsec.Char (char, space, string)


main :: IO ()
main = run 3003 serve
  where
    serve request respond = do
        print $ rawPathInfo request
        print =<< strictRequestBody request
        response <- case rawPathInfo request of
            "/post" -> page . Just . parseNumber <$> strictRequestBody request
            _       -> return $ page Nothing
        respond $ responseLBS status200 [(hContentType, "text/html")] $ renderBS response

parseNumber :: LByteString -> Html ()
parseNumber input = div_ $ do
    h1_ "Ergebnis:"
    let input = decodeText . toText $ input
    --     input' = stripPrefix "number=" input ?: input
    -- case parse pNumber "Eingabe" input of
    --     Right s -> code_ . toHtml $ (show s :: String)
    --     Left e -> code_ . toHtml $ errorBundlePretty e
    code_ $ toHtml input

data LandISOCode = DE deriving (Show)

data Number = Number
    { landVorwahl  :: LandISOCode
    , stadtVorwahl :: Text
    , num          :: Text
    , durchwahl    :: Maybe Text
    }
    deriving (Show)

type Parser = Parsec Void Text

pNumber :: Parser Number
pNumber = do
    vorwahl <- optional $ DE <$ "+49"
    -- let a = (if isJust vorwahl then option [] (pure <$> char '0') else pure <$> char '0')
    -- city <- (++) <$> a <*> count 4 (satisfy isNumber)
    -- optional sep
    -- num <- count 6 (satisfy isNumber)
    -- durchwahl <- optional $ optional sep *> count 3 (satisfy isNumber)
    return $ Number (vorwahl ?: DE) "" "" (Just "") -- (pack city) (pack num) (pack <$> durchwahl)

sep :: Parser ()
sep = void $ oneOf ['/', '-']

page :: Maybe (Html ()) -> Html ()
page solution = html_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        title_ "Telefonnummer"
    body_ $ do
        fromMaybe mempty solution
        br_ []
        h1_ "Telefonnummer eingeben:"
        form_ [method_ "POST", action_ "/post"] $ do
            input_ [name_ "number", type_ "tel"]
            input_ [type_ "submit"]
