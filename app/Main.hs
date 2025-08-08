{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Parser.HttpRequest
import System.Console.ANSI
import System.Directory
import System.Environment
import System.IO (BufferMode (NoBuffering), IOMode (ReadMode), hGetContents, hSetBuffering, openFile, stderr, stdout)
import qualified Text.Megaparsec as MP

validEncodings :: [BC.ByteString]
validEncodings = ["gzip"]

route :: NS.Socket -> HttpRequest -> Maybe String -> IO ()
route socket (HttpRequest{_method = "POST", _target = path, _headers = hs, _body = (Just fileContents)}) fp
    | BC.isPrefixOf "/files/" path = do
        case BC.stripPrefix "/files/" path of
            Just fileName -> case fp of
                Nothing -> sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
                Just pathStr -> do
                    let filePath = pathStr ++ BC.unpack fileName
                    writeFile filePath $ BC.unpack fileContents
                    sendAll socket (BC.pack "HTTP/1.1 201 Created\r\n\r\n")
            Nothing -> sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
route socket (HttpRequest{_method = "GET", _target = path, _headers = hs}) fp
    | path == "/" = sendAll socket (BC.pack "HTTP/1.1 200 OK\r\n\r\n")
    | path == "/user-agent" = do
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
    | BC.isPrefixOf "/echo/" path = do
        case BC.stripPrefix "/echo/" path of
            Just str -> do
                print filteredHeaders
                print splitHeaders
                print splitHeaders'
                print valid
                let resp =
                        "HTTP/1.1 200 OK\r\n"
                            <> "Content-Type: text/plain\r\n"
                            <> contentEncodingStr
                            <> "Content-Length: "
                            <> show (BC.length str)
                            <> "\r\n\r\n"
                            <> BC.unpack str
                sendAll socket (BC.pack resp)
              where
                filteredHeaders = filter (\x -> _name x == "Accept-Encoding") hs
            splitHeaders = if not (null filteredHeaders) then BC.split ',' (_value $ head filteredHeaders) else []
                splitHeaders' = map (BC.filter (/= ' ')) splitHeaders
                valid = filter (`elem` validEncodings) splitHeaders'
                contentEncodingStr =
                    if not (null filteredHeaders) && not (null valid)
                        then BC.unpack ("Content-Encoding: " <> head valid <> "\r\n")
                        else BC.unpack ""
            Nothing -> sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
    | BC.isPrefixOf "/files/" path = do
        case BC.stripPrefix "/files/" path of
            Just str -> case fp of
                Nothing -> sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
                Just pathStr -> do
                    let filePath = pathStr ++ BC.unpack str
                    exists <- doesFileExist filePath
                    if exists
                        then do
                            handle <- openFile filePath ReadMode
                            contents <- hGetContents handle
                            let resp =
                                    "HTTP/1.1 200 OK\r\n"
                                        <> "Content-Type: application/octet-stream\r\n"
                                        <> "Content-Length: "
                                        <> show (length contents)
                                        <> "\r\n\r\n"
                                        <> contents
                            sendAll socket (BC.pack resp)
                        else sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
            Nothing -> sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")
route socket _ fp = sendAll socket (BC.pack "HTTP/1.1 404 Not Found\r\n\r\n")

main :: IO ()
main = do
    args <- getArgs
    let flags = zip args (drop 1 args)
    let dir = lookup "--directory" flags

    putStrLn "args:"
    print args
    print dir

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
                Right httpRequest -> route clientSocket httpRequest dir
                Left errorBundle -> print errorBundle

            NS.close clientSocket
