{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Router (route)
where

import qualified Codec.Compression.GZip as GZip (compress)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, get, modify, put)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BCL
import Http.Types (Config, ConnectionState (..), HeaderName, HeaderValue, HttpRequest (..), HttpResponse (..))

notFoundResponse :: HttpResponse
notFoundResponse = HttpResponse 404 "Not Found" [] Nothing

okResponse :: [(HeaderName, HeaderValue)] -> BC.ByteString -> HttpResponse
okResponse headers body = HttpResponse 200 "OK" headers (Just body)

route ::
    (MonadReader Config m, MonadState ConnectionState m, MonadIO m) =>
    HttpRequest ->
    m HttpResponse
route request@(HttpRequest{_method = "GET", _target = path})
    | path == "/" = pure $ okResponse [] ""
    | path == "/user-agent" = handleUserAgent request
    | "/echo/" `BC.isPrefixOf` path = handleEcho request
    | "/files/" `BC.isPrefixOf` path = handleFiles request
    | otherwise = pure notFoundResponse
route _ = pure notFoundResponse

handleUserAgent :: Monad m => HttpRequest -> m HttpResponse
handleUserAgent (HttpRequest{_headers = hs}) = pure $
    case lookup "User-Agent" hs of
        Just ua -> okResponse [("Content-Type", "text/plain")] ua
        Nothing -> okResponse [("Content-Type", "text/plain")] ""

handleEcho :: Monad m => HttpRequest -> m HttpResponse
handleEcho HttpRequest{_headers, _target} = do
    case BC.stripPrefix "/echo/" _target of
        Just str -> do
            let (encodingHeader, compressedBody) = handleEncoding _headers str
            pure $ okResponse (("Content-Type", "text/plain") : encodingHeader) compressedBody
        Nothing -> pure notFoundResponse

handleEncoding :: [(HeaderName, HeaderValue)] -> BC.ByteString -> ([(HeaderName, HeaderValue)], BC.ByteString)
handleEncoding headers body =
    case lookup "Accept-Encoding" headers of
        Just encodings
            | "gzip" `BC.isInfixOf` encodings ->
                let compressed = BCL.toStrict . GZip.compress . BCL.fromStrict $ body
                 in ([("Content-Encoding", "gzip")], compressed)
        _ -> ([], body)

handleFiles :: (MonadReader Config m, MonadIO m) => HttpRequest -> m HttpResponse
handleFiles request = do
    config <- ask
    liftIO $ print config
    undefined
