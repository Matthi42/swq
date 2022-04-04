{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Char                 (isNumber)
import           Data.Text                 (pack, stripPrefix)
import           Lucid
import           Network.HTTP.Types        (methodGet, methodPost, status200,
                                            status404, Status)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Parse
import           Prelude                   hiding (length)
import           Types

-- import           Text.Megaparsec      hiding (chunk)
-- import           Text.Megaparsec.Char (char, space, string)

----------------------------------------------------- TYPES -----------------------------------------------------

main :: IO ()
respondHtml :: Status -> Html () -> Response
runNumberApp :: DB -> Text -> Request -> IO (Status, Html ())

type DB = IORef [Number]
data NumberRequest
    = GetHTML
    | PostNumber ByteString
    | WrongEndpoint

type NumberResult = Either NumberError Number
data NumberError
    = IllegalChars String
    | IncorrectLength Int
    | UnknownCountryCode String
    deriving (Show)

type LandISOCode = String
type NumberString = String
data Number = Number
    { countryCode :: LandISOCode
    , areaCode    :: String
    , mainNumber  :: String
    , extension   :: Maybe String
    }
    deriving (Show)

parseRequest :: Request -> IO NumberRequest
processRequest :: DB -> Text -> NumberRequest -> IO (Status, Html ())

parseNumber :: ByteString -> NumberResult

renderResultAndForm :: [Number] -> Maybe NumberResult -> Html ()
renderError :: NumberError -> Html ()
renderResult :: Number -> Html ()
renderNumber :: Number -> Html ()


------------------------------------------------ IMPLEMENTATION -------------------------------------------------

main = do
    allNumbers <- newIORef []
    css <- readFileText "./style.css"
    run 3003 $ \request respond -> do
        -- print $ rawPathInfo request
        -- print =<< strictRequestBody request
        htmlResult <- runNumberApp allNumbers css request
        respond (uncurry respondHtml htmlResult)

runNumberApp allNumbers css = processRequest allNumbers css <=< parseRequest
respondHtml status = responseLBS status [(hContentType, "text/html")] . renderBS

parseRequest request = toNumRequest <$> parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd request
  where
    correctPath :: ([Param], b) -> NumberRequest
    correctPath body
        | requestMethod request == methodGet = GetHTML
        | requestMethod request == methodPost =
            case find ((== "number") . fst) (fst body) of
                Just (_, rawNumber) -> PostNumber rawNumber
                Nothing             -> WrongEndpoint -- MAYBE better error
        | otherwise = WrongEndpoint
-- GET url.example.com/ --> HTML
-- POST url.example.com/ --> parse -> HTML mit Number
-- everything else -> 404
    toNumRequest body = case rawPathInfo request of
        "/" -> correctPath body
        ""  -> correctPath body
        _   -> WrongEndpoint

processRequest db css GetHTML =  (status200,) . page css . flip renderResultAndForm Nothing <$> readIORef db
processRequest _  css WrongEndpoint = return . (status200,) . page css $ h1_ "Page not found"
processRequest db css (PostNumber rawNumber) = do
    let result = parseNumber rawNumber
        update = case result of
            Right newNumber -> (:) newNumber
            Left _ -> id
    numbers <- atomicModifyIORef db (update &&& id)
    print rawNumber
    print result
    print numbers
    return . (status200,) . page css $ renderResultAndForm numbers (Just result)

parseNumber = const . return $ Number "+49" "074538" "77719" (Just "15")

renderResultAndForm numbers solution = do
        h1_ "Telefonnummern"
        form_ [method_ "POST", action_ "/"] $ do
            label_ [class_ "heading"] "Telefonnummer eingeben:"
            input_ [name_ "number", type_ "tel"]
            input_ [type_ "submit", class_ "button"]
        case solution of
            Nothing -> mempty
            Just (Left error) -> renderError error
            Just (Right result) -> renderResult result
        ul_ [class_ "num-list"] $ do
            forM_ numbers (li_ [class_ "number"] . renderNumber)

renderNumber (Number cc ac mn e) =
    do
        span_ $ toHtml cc
        span_ $ toHtml ac
        span_ $ toHtml mn
        case e of
            (Just ex) -> span_ $ toHtml ex
            Nothing -> span_ "-"
--Lars
renderError (IllegalChars chars) = do
    p_ [class_ "error"] $ do
        span_ "Fehler: Es wurden nicht erlaubte Character gefunden."
        span_ . show $ chars
renderError (IncorrectLength number) = do
    p_ [class_ "error"] $ do
        span_ "Fehler: Die Nummer kann keine authentitäre Nummer sein, da sie entweder zu lang oder zu kurz ist."
        span_ . show $ number
renderError (UnknownCountryCode chars) = do
    p_ [class_ "error"] $ do
        span_ "Fehler: Es wurde kein Land zu der ausgewählten Nummer gefunden."
        span_ . show $ chars

renderResult result = do
    p_ [class_ "number"]$ do
        span_ [class_ "heading"] "Ergebnis: "
        renderNumber result

page :: Text -- ^ CSS-Datei
  -> Html () -- ^ Html body
  -> Html () 
page css body = html_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        style_ css
        title_ "Telefonnummer"
        link_ [href_ "https://fonts.googleapis.com/css?family=Butterfly+Kids|Roboto",rel_ "stylesheet"]
    body_ body
