{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Char                 (isDigit)
import           Data.Text                 (pack, stripPrefix, unpack)
import           Lucid
import           Network.HTTP.Types        (Status, methodGet, methodPost,
                                            status200, status404)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Parse
import           Prelude                   hiding (length, some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- import           Text.Regex.TDFA
import           Types
import           System.Environment

-- import           Text.Megaparsec      hiding (chunk)
-- import           Text.Megaparsec.Char (char, space, string)

----------------------------------------------------- TYPES -----------------------------------------------------

type DB = IORef [Number]
data NumberRequest
    = GetHTML
    | PostNumber Text
    | WrongEndpoint

type NumberResult = Either NumberError Number
data NumberError
    = IllegalChars Text
    | IncorrectLength Int
    | UnknownCountryCode Text
    deriving (Show)

type LandISOCode = (Text, Text)
type NumberString = String
data Number = Number
    { countryCode :: LandISOCode
    , areaCode    :: Text
    , mainNumber  :: Text
    , extension   :: Maybe Text
    }
    deriving (Show)

main :: IO ()
respondHtml :: Status -> Html () -> Response
runNumberApp :: DB -> Text -> Request -> IO (Status, Html ())
parseRequest :: Request -> IO NumberRequest
processRequest :: DB -> Text -> NumberRequest -> IO (Status, Html ())
parseNumber :: Text -> NumberResult
renderResultAndForm :: [Number] -> Maybe NumberResult -> Html ()
renderError :: NumberError -> Html ()
renderResult :: Number -> Html ()
renderNumber :: Number -> Html ()
------------------------------------------------ IMPLEMENTATION -------------------------------------------------

main = do
    args <- getArgs
    css <- readFileText $ listToMaybe args ?: "./style.css"
    allNumbers <- newIORef []
    run 2804 $ \request respond -> do
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
                Just (_, rawNumber) -> case decodeUtf8Strict rawNumber of
                    Right num -> PostNumber num
                    Left err  -> WrongEndpoint
                Nothing -> WrongEndpoint -- MAYBE better error
        | otherwise = WrongEndpoint
    -- GET url.example.com/ --> HTML
    -- POST url.example.com/ --> parse -> HTML mit Number
    -- everything else -> 404
    toNumRequest body = case rawPathInfo request of
        "/" -> correctPath body
        ""  -> correctPath body
        _   -> WrongEndpoint

processRequest db css GetHTML = (status200,) . page css . flip renderResultAndForm Nothing <$> readIORef db
processRequest _ css WrongEndpoint = return . (status200,) . page css $ h1_ "Page not found"
processRequest db css (PostNumber rawNumber) = do
    let result = parseNumber rawNumber
        update = case result of
            Right newNumber -> (:) newNumber
            Left _          -> id
    numbers <- atomicModifyIORef db (update &&& id)
    -- print rawNumber
    -- print result
    -- print numbers
    return . (status200,) . page css $ renderResultAndForm numbers (Just result)

renderResultAndForm numbers solution = do
    h1_ "Telefonnummern"
    form_ [method_ "POST", action_ "/"] $ do
        label_ [class_ "heading"] "Telefonnummer eingeben:"
        input_ [name_ "number", type_ "tel"]
        input_ [type_ "submit", class_ "button"]
    case solution of
        Nothing             -> mempty
        Just (Left error)   -> renderError error
        Just (Right result) -> renderResult result
    ul_ [class_ "num-list"] $ do
        forM_ numbers (li_ [class_ "number"] . renderNumber)

renderNumber (Number cc ac mn e) = do
    span_ . toHtml . fst $ cc
    span_ $ toHtml ac
    span_ $ toHtml mn
    case e of
        (Just ex) -> span_ $ toHtml ex
        Nothing   -> span_ "-"
    span_ . toHtml $ "(" <> snd cc <> ")"
    a_ [href_ $ "tel:" <> fst cc <> "-" <> ac <> "-" <> mn <> maybe "" ("-" <>) e] "📞"

renderError (IllegalChars chars) = do
    div_ [class_ "error"] $ do
        p_ "Fehler: Es wurden nicht erlaubte Zeichen gefunden."
        pre_ . toHtml $ chars
renderError (IncorrectLength number) = do
    p_ [class_ "error"] $ do
        span_ "Fehler: Die Nummer kann keine authentitäre Nummer sein, da sie entweder zu lang oder zu kurz ist."
        span_ . show $ number
renderError (UnknownCountryCode chars) = do
    p_ [class_ "error"] $ do
        span_ "Fehler: Es wurde kein Land zu der ausgewählten Nummer gefunden."
        span_ . toHtml $ chars

renderResult result = do
    p_ [class_ "number"] $ do
        span_ [class_ "heading"] "Ergebnis: "
        renderNumber result

page ::
    -- | CSS-Datei
    Text ->
    -- | Html body
    Html () ->
    Html ()
page css body = html_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        style_ css
        title_ "Telefonnummer"
    body_ body

----------------------------------------------------- PARSER -----------------------------------------------------

-- https://github.com/robinheghan/elm-phone-numbers/blob/master/src/PhoneNumber/Countries.elm#L3005
-- numberRegex :: ByteString
-- numberRegex = "(?:32|49[4-6]\\d)"
-- -- (?:32|49[4-6]\\d)\\d{9}|49[0-7]\\d{3,9}|(?:[34]0|[68]9)\\d{3,13}|(?:2(?:0[1-689]|[1-3569]\\d|4[0-8]|7[1-7]|8[0-7])|3(?:[3569]\\d|4[0-79]|7[1-7]|8[1-8])|4(?:1[02-9]|[2-48]\\d|5[0-6]|6[0-8]|7[0-79])|5(?:0[2-8]|[124-6]\\d|[38][0-8]|[79][0-7])|6(?:0[02-9]|[1-358]\\d|[47][0-8]|6[1-9])|7(?:0[2-8]|1[1-9]|[27][0-7]|3\\d|[4-6][0-8]|8[0-5]|9[013-7])|8(?:0[2-9]|1[0-79]|2\\d|3[0-46-9]|4[0-6]|5[013-9]|6[1-8]|7[0-8]|8[0-24-6])|9(?:0[6-9]|[1-4]\\d|[589][0-7]|6[0-8]|7[0-467]))\\d{3,12}

-- parseNumber raw =
--     let match = raw =~ numberRegex :: Bool
--     in return $ Number (show match) "" "" Nothing
-- parseNumber = const . return $ Number ("+49", "DE") "074538" "77719" (Just "15")
parseNumber num = do
    (cc, ac, mn, ex) <- first (IllegalChars . pack . errorBundlePretty) $ parse pNumber (unpack num) num
    cc <- case cc of
        Just "+49" -> return ("+49", "DE")
        Just "+1" -> return ("+1", "US/CA")
        Just "+52" -> return ("+52", "MX")
        Just "+33" -> return ("+33", "FR")
        Just "+32" -> return ("+32", "BE")
        Just "+44" -> return ("+44", "UK")
        Just "+91" -> return ("+91", "IN")
        Just cc    -> Left . UnknownCountryCode $ cc
        Nothing    -> return ("+49", "DE")
    return $ Number cc ac mn ex

type Parser = Parsec Void Text

pNumber :: Parser (Maybe Text, Text, Text, Maybe Text)
pNumber =
    let optionallyInBrackets :: Parser p -> Parser p
        optionallyInBrackets p =
            try $
                choice
                    [ p
                    , between "(" ")" p
                    , between "[" "]" p
                    ]
        sep :: Parser (Maybe Char)
        sep =
            optional $
                choice
                    [ char ' '
                    , char '/'
                    , char '-'
                    ]
        pCC = pack <$> ((:) <$> char '+' <*> count' 1 2 digitChar)
        pLeading0 :: Bool -> Parser String
        pLeading0 True =
            label "Leading zero" $
                optionallyInBrackets (pure <$> char '0')
                    <|> return []
        pLeading0 False = optionallyInBrackets (pure <$> char '0')
        pAC = count' 2 4 digitChar
        pEx = pack <$> count' 1 3 digitChar
     in do
            cc <- optional (optionallyInBrackets pCC) <?> "Ländercode"
            sep
            ac <-
                label "Vorwahl" . optionallyInBrackets $
                    (++)
                        <$> try (pLeading0 (isJust cc))
                        <*> pAC
            sep
            mn <-
                label "Hauptnummer" . optionallyInBrackets $
                    filter (\c -> c /= '/' && c /= '-')
                        <$> some (satisfy $ \c -> isDigit c || c == '/' || c == '-')
            sep
            ex <- optional (optionallyInBrackets pEx) <?> "Durchwahl"
            return (cc, pack ac, pack mn, ex)

----------------------------------------------------- TEST -----------------------------------------------------

testData =
    [ ("+49 0201 123456", True)
    , ("+44 0201123456", True)
    , ("0033 0201/123456", True)
    , ("0049201123456", True)
    , ("(0)201 1234 56", True)
    , ("+49 (941) 790-4780", True)
    , ("015115011900", True)
    , ("+91 09870987 899", True)
    , ("[+49] (0)89-800/849-50", True)
    , ("+49 (8024) [990-477]", True)
    ]

printFailed :: IO ()
printFailed = mapM_ (print' . parseNumber . fst) testData
  where
    print' (Left (IllegalChars err)) = putTextLn err
    print' (Left err)                = print err
    print' _                         = return ()

printAll :: IO ()
printAll = mapM_ (uncurry (>>) . (print &&& print' . parseNumber) . fst) testData
  where
    print' (Left (IllegalChars err)) = putTextLn err
    print' (Left err)                = print err
    print' (Right suc)               = print suc

test = all (\(num, res) -> isRight (parseNumber num) == res) testData
