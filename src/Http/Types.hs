{-# LANGUAGE DuplicateRecordFields #-}

module Http.Types (HttpRequest (..), HttpResponse (..), HeaderName, HeaderValue, isHeaderPresent, isHeaderValuePresent)
where

import qualified Data.ByteString.Char8 as BC

-- data Header = Header
--    { _name :: BC.ByteString
--    , _value :: BC.ByteString
--    }
--    deriving (Show)

data HttpRequest = HttpRequest
    { _method :: BC.ByteString
    , _target :: BC.ByteString
    , _version :: BC.ByteString
    , _headers :: [(BC.ByteString, BC.ByteString)]
    , _body :: Maybe BC.ByteString
    }
    deriving (Show)

data HttpResponse = Response
    { _statusCode :: Int
    , _statusText :: BC.ByteString
    , _headers :: [(BC.ByteString, BC.ByteString)]
    , _body :: BC.ByteString
    }

type HeaderName = BC.ByteString
type HeaderValue = BC.ByteString

isHeaderPresent :: HttpRequest -> HeaderName -> Bool
isHeaderPresent (HttpRequest{_headers = []}) _ = False
isHeaderPresent (HttpRequest{_headers = hs}) headerName = any (\x -> fst x == headerName) hs

isHeaderValuePresent :: HttpRequest -> HeaderName -> HeaderValue -> Bool
isHeaderValuePresent (HttpRequest{_headers = []}) _ _ = False
isHeaderValuePresent (HttpRequest{_headers = hs}) headerName headerValue = any (\x -> fst x == headerName && snd x == headerValue) hs
