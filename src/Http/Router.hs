{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Router (route)
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, get, modify, put)
import qualified Data.ByteString.Char8 as BC
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
handleEcho request = undefined

handleFiles :: (MonadReader Config m, MonadIO m) => HttpRequest -> m HttpResponse
handleFiles request = do
    config <- ask
    liftIO $ print config
    undefined
