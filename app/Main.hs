{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
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

    -- Stream sockets use the Transmission Control Protocol (TCP) for communication.
    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
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
        -- The return value is a pair (conn, address) where conn is a new socket object usable to send and
        -- receive data on the connection, and address is the address bound to the socket on the other end of the connection.
        (clientSocket, clientAddr) <- accept serverSocket

        request <- recv clientSocket 4096
        BC.putStrLn $ "Received " <> BC.pack (show (BC.length request)) <> " bytes from " <> BC.pack (show clientAddr) <> "."
        setSGR [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity]
        BC.putStrLn $ BC.pack (show request)
        setSGR [Reset]

        let (method : target : version : xs) = BC.words request
        case target of
            "/" -> sendAll clientSocket (BC.pack "HTTP/1.1 200 OK\r\n\r\n")
            _ -> sendAll clientSocket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")

        -- let resp = "HTTP/1.1 200 OK\r\n\r\n"
        -- sendAll clientSocket (BC.pack resp)

        close clientSocket
