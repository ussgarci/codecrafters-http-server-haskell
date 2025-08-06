{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Map as M
import Data.Void (Void)
import Data.Word (Word8)
import Debug.Trace
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
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
    -- , _body :: Maybe BC.ByteString
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

parseBody :: Parser BC.ByteString
parseBody = undefined

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
    return $ HttpRequest method target version headers

route :: NS.Socket -> HttpRequest -> IO ()
route socket (HttpRequest{_method = "GET", _target = "/"}) = sendAll socket (BC.pack "HTTP/1.1 200 OK\r\n\r\n")
route socket (HttpRequest{_method = "GET", _target = "/user-agent", _headers = hs}) = do
    let resp =
            "HTTP/1.1 200 OK\r\n"
                <> "Content-Type: text/plain\r\n"
                <> "Content-Length: "
                <> show (BC.length (_value (head uaHeader)))
                <> "\r\n\r\n"
                <> BC.unpack (_value (head uaHeader))
          where
            uaHeader = filter (\x -> _name x == "User-Agent") hs
    sendAll socket (BC.pack resp)
route socket (HttpRequest{_method = "GET", _target = path}) = case BC.stripPrefix "/echo/" path of
    (Just str) ->
        do
            let resp =
                    "HTTP/1.1 200 OK\r\n"
                        <> "Content-Type: text/plain\r\n"
                        <> "Content-Length: "
                        <> show (BC.length str)
                        <> "\r\n\r\n"
                        <> BC.unpack str

            setSGR [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity]
            BC.putStrLn $ BC.pack (show resp)
            sendAll socket (BC.pack resp)
    Nothing -> sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
route socket _ = sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
        port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    -- Get address information for the given host and port
    -- getAddrInfo :: GetAddrInfo t => Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName -> IO (t AddrInfo)
    addrInfo <- NS.getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- NS.socket (NS.addrFamily $ head addrInfo) NS.Stream NS.defaultProtocol
    NS.setSocketOption serverSocket NS.ReuseAddr 1 -- avoid TIME_WAIT
    NS.bind serverSocket $ NS.addrAddress $ head addrInfo

    -- The second argument specifies the maximum number of queued connections and should be at least 1;
    -- the maximum value is system-dependent (usually 5).
    NS.listen serverSocket 5

    -- forever     :: (Applicative f) => f a -> f b
    -- forever a   = let a' = a *> a' in a'
    --    a' = a *> a'
    --    a' = a *> (a *> a')
    --    a' = a *> (a *> (a *> a'))
    --    a' = a *> (a *> (a *> ... )))
    forever $ do
        (clientSocket, clientAddr) <- NS.accept serverSocket
        request <- recv clientSocket 4096
        let requestLength = BC.length request

        BC.putStrLn $ "Received " <> BC.pack (show requestLength) <> " bytes from " <> BC.pack (show clientAddr) <> "."
        setSGR [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity]
        BC.putStrLn $ BC.pack (show request)
        setSGR [Reset]

        case MP.runParser parseHttpRequest "" request of
            Right httpRequest -> route clientSocket httpRequest
            Left errorBundle -> print errorBundle

        NS.close clientSocket
