{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Parser.HttpRequest
import System.Console.ANSI
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import qualified Text.Megaparsec as MP

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

    forever $ do
        (clientSocket, clientAddr) <- NS.accept serverSocket
        
        forkIO $ do
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
