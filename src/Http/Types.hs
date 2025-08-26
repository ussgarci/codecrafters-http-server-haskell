{-# LANGUAGE DuplicateRecordFields #-}

module Http.Types (HttpRequest (..), HttpResponse (..), Config (..), ConnectionState (..), HeaderName, HeaderValue, isHeaderPresent, isHeaderValuePresent)
where

import qualified Data.ByteString.Char8 as BC
import Data.Time.Clock

data Config = Config
    { directory :: FilePath
    }
    deriving (Show)

data ConnectionState = ConnectionState
    { requestsHandled :: Int
    , connectionOpened :: UTCTime
    }
    deriving (Show)

data HttpRequest = HttpRequest
    { _method :: BC.ByteString
    , _target :: BC.ByteString
    , _version :: BC.ByteString
    , _headers :: [(BC.ByteString, BC.ByteString)]
    , _body :: Maybe BC.ByteString
    }
    deriving (Show)

data HttpResponse = HttpResponse
    { _statusCode :: Int
    , _statusText :: BC.ByteString
    , _headers :: [(BC.ByteString, BC.ByteString)]
    , _body :: Maybe BC.ByteString
    }

type HeaderName = BC.ByteString
type HeaderValue = BC.ByteString

isHeaderPresent :: HttpRequest -> HeaderName -> Bool
isHeaderPresent (HttpRequest{_headers = []}) _ = False
isHeaderPresent (HttpRequest{_headers = hs}) headerName = any (\x -> fst x == headerName) hs

isHeaderValuePresent :: HttpRequest -> HeaderName -> HeaderValue -> Bool
isHeaderValuePresent (HttpRequest{_headers = []}) _ _ = False
isHeaderValuePresent (HttpRequest{_headers = hs}) headerName headerValue = any (\x -> fst x == headerName && snd x == headerValue) hs
