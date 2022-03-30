{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Data.Text                 (pack, replace, stripSuffix, unpack)
import           Lucid
import           Network.HTTP.Types        (status200, status404)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, Response, rawPathInfo,
                                            responseFile, responseLBS, strictRequestBody)
import           Network.Wai.Handler.Warp  (run)
import           Prelude                   hiding (ByteString, length)
import           Types

main :: IO ()
main = run 3003 serve
  where
    serve request respond = do
        response <- case rawPathInfo request of
                "/post" -> page . Just . parseNumber <$> strictRequestBody request 
                _       -> return $ page Nothing
        respond $ responseLBS status200 [(hContentType, "text/html")] $ renderBS response

parseNumber :: LByteString -> Html ()
parseNumber input = div_ $ do 
   h1_ "Ergebnis:"
   p_ "TODO"

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
