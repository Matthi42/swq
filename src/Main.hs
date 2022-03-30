{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Text                       (pack, replace, stripSuffix,
                                                  unpack)
import           Lucid                           (Html, renderBS)
import           Network.HTTP.Types              (status200, status404)
import           Network.HTTP.Types.Header       (hContentType)
import           Network.Wai                     (Application, Response,
                                                  rawPathInfo, responseFile,
                                                  responseLBS)
import           Network.Wai.Handler.Warp        (run)
import           Prelude                         hiding (ByteString, length)
import           Types

main :: IO ()
main = run 3003 serve where
    serve request respond = respond =<< selectRoute

    sanitizedPath = B.stripPrefix "/" (rawPathInfo request) ?: B.empty
    segments = B.split '/' sanitizedPath
    htmlOk = responseLBS status200 [(hContentType, "text/html")] . renderBS
    html404 = responseLBS status404 [(hContentType, "text/html")] (renderBS err404)
    findSong :: B.ByteString -> Maybe B.ByteString -> Response
    findSong name key = htmlOk $
        fromMaybe err404 $ do
            song' <- find ((== B.unpack name) . chordProFile) songs
            key' <- maybe (Just Nothing) (Just . parseKeyMaybe . replace "%23" "#" . decodeUtf8) key
            return $ song env song' key'
    selectRoute = do
        print segments
        return $ case segments of
            [] -> htmlOk $ mainpage env
            ["songs", name] -> findSong name Nothing
            ["songs", name, key] -> findSong name (if B.null key then Nothing else Just key)
            ["favicon.ico"] ->
                case favicon of
                    Just path -> responseFile status200 [(hContentType, "image/x-icon")] path Nothing
                    Nothing -> html404
            _ -> html404

