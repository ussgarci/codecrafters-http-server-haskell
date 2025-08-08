{-# LANGUAGE OverloadedStrings #-}

module Parser.HttpRequest (HttpRequest (..), Header (..), parseHttpRequest)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Map as M
import Data.Void (Void)
import Data.Word (Word8)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MPB

data Header = Header
    { _name :: BC.ByteString
    , _value :: BC.ByteString
    }
    deriving (Show)

data HttpRequest = HttpRequest
    { _method :: BC.ByteString
    , _target :: BC.ByteString
    , _version :: BC.ByteString
    , _headers :: [Header]
    , _body :: Maybe BC.ByteString
    }
    deriving (Show)

type Parser = MP.Parsec Void BC.ByteString

charCodesMap :: M.Map Char Word8
charCodesMap =
    M.fromList
        [ (' ', fromIntegral (ord ' ') :: Word8)
        , (':', fromIntegral (ord ':') :: Word8)
        , ('\r', fromIntegral (ord '\r') :: Word8)
        ]

parseHeader :: Parser Header
parseHeader = do
    name <- MP.takeWhile1P (Just "http header name") (/= (charCodesMap M.! ':'))
    _ <- MPB.char (charCodesMap M.! ':')
    _ <- MPB.space
    value <- MP.takeWhile1P (Just "http header value") (/= (charCodesMap M.! '\r'))
    _ <- MPB.crlf
    return $ Header name value

parseHttpRequest :: Parser HttpRequest
parseHttpRequest = do
    MPB.space
    -- change to MPB.takeWhile1P (Just "http method") BC8.isLetter_w8
    -- method <- MPB.string "GET"
    method <- B.pack <$> MP.many MPB.letterChar
    MPB.space
    target <- MP.takeWhile1P (Just "http request target") (/= (charCodesMap M.! ' '))
    MPB.space
    version <- MPB.string "HTTP/1.1"
    _ <- MPB.crlf
    headers <- MP.manyTill parseHeader MPB.crlf
    body <- MP.takeRest
    if BC.null body then
        return $ HttpRequest method target version headers Nothing
    else
        return $ HttpRequest method target version headers (Just body)
