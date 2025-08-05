{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

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
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1 -- avoid TIME_WAIT
    bind serverSocket $ addrAddress $ head addrInfo

    -- The second argument specifies the maximum number of queued connections and should be at least 1;
    -- the maximum value is system-dependent (usually 5).
    listen serverSocket 5

    -- forever     :: (Applicative f) => f a -> f b
    -- forever a   = let a' = a *> a' in a'
    --    a' = a *> a'
    --    a' = a *> (a *> a')
    --    a' = a *> (a *> (a *> a'))
    --    a' = a *> (a *> (a *> ... )))
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        request <- recv clientSocket 4096
        let requestLength = BC.length request

        BC.putStrLn $ "Received " <> BC.pack (show requestLength) <> " bytes from " <> BC.pack (show clientAddr) <> "."
        setSGR [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity]
        BC.putStrLn $ BC.pack (show request)
        setSGR [Reset]

        case BC.words request of
            ("GET" : "/" : _) -> sendAll clientSocket (BC.pack "HTTP/1.1 200 OK\r\n\r\n")
            ("GET" : path : _) -> case BC.stripPrefix "/echo/" path of
                (Just str) ->
                    do
                        let resp =
                                "HTTP/1.1 200 OK\r\n"
                                    <> "Content-Type: text/plain\r\n"
                                    <> "Content-Length: "
                                    <> (show $ BC.length str)
                                    <> "\r\n"
                                    <> BC.unpack str
                        
                        BC.putStrLn $ "Sending " <> BC.pack (show $ BC.length str) <> " bytes to " <> BC.pack (show clientAddr) <> "."
                        setSGR [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity]
                        BC.putStrLn $ BC.pack (show resp)
                        sendAll clientSocket (BC.pack resp)
                Nothing -> sendAll clientSocket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
            _ -> sendAll clientSocket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")

        close clientSocket
