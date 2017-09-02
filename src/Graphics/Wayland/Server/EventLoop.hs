{-# LANGUAGE RecordWildCards #-}

module Graphics.Wayland.Server.EventLoop where

import Data.IORef
import Data.Bits
import Control.Exception.Base
import Foreign.C.Types

import qualified Data.IntMap as M
--import qualified Data.IntSet as S

import Graphics.Wayland.Server.Client
import Graphics.Wayland.System.Socket
--import qualified Graphics.Wayland.Internal.Socket as Sock
import qualified Graphics.Wayland.System.Epoll as Epoll

data EventLoop = EventLoop
    { epoll :: Epoll.Device
    --, listenSocks :: IORef S.IntSet
    , clientSocks :: IORef (M.IntMap ClientConn)
    }

data ClientConn = ClientConn
    { clientSocket :: Socket
    , processId :: CUInt
    , userId :: CUInt
    , groupId :: CUInt
    -- , processCmdline :: String
    } deriving (Show)

createEventLoop :: IO EventLoop
createEventLoop = do
    epoll <- Epoll.create [Epoll.Cloexec] 128
    --listenSocks <- newIORef S.empty
    clientSocks <- newIORef M.empty
    return EventLoop{..}

closeEventLoop :: EventLoop -> IO ()
closeEventLoop EventLoop{..} = return () -- TODO

addListenSocket :: EventLoop -> Socket -> IO ()
addListenSocket EventLoop{..} (Socket fd) = do
    Epoll.add epoll [Epoll.InEvent] fd
    --modifyIORef listenSocks (S.insert (fromIntegral fd))

removeListenSocekt :: EventLoop -> Socket -> IO ()
removeListenSocekt EventLoop{..} (Socket fd) = do
    Epoll.delete epoll fd
    --modifyIORef listenSocks (S.delete (fromIntegral fd))

addClientSocket :: EventLoop -> Socket -> IO ClientConn
addClientSocket EventLoop{..} sock@(Socket fd) = do
    client <- newClientConn sock
    Epoll.add epoll [Epoll.InEvent, Epoll.OutEvent] fd
    modifyIORef clientSocks (M.insert (fromIntegral fd) client)
    return client

removeClientConn :: EventLoop -> ClientConn -> IO ()
removeClientConn EventLoop{..} ClientConn{clientSocket = Socket fd} = do
    Epoll.delete epoll fd
    modifyIORef clientSocks (M.delete (fromIntegral fd))

newListenSocket :: EventLoop -> String -> IO Socket
newListenSocket el path = do
    sock <- socket (sockStream .|. sockNonblock .|. sockCloexec)
    (bind sock path >> addListenSocket el sock) `onException` close sock
    return sock

acceptClient :: EventLoop -> Socket -> IO ClientConn
acceptClient el sock = do
    clientSock <- accept sock (sockNonblock .|. sockCloexec)
    addClientSocket el clientSock

data Event = Connect ClientConn
           | Disconnect ClientConn
           | ReadAvailable ClientConn
           | WriteAvailable ClientConn

processEpollEvent :: EventLoop -> Epoll.Event -> IO [Event]
processEpollEvent el@EventLoop{..} (Epoll.Event fd evs) = do
    mc <- M.lookup (fromIntegral fd) <$> readIORef clientSocks
    case mc of
        Nothing -> sequence [ Connect <$> acceptClient el (Socket fd)
                                | Epoll.InEvent `elem` evs ]
        Just client -> return $
            if Epoll.HangupEvent `elem` evs
               then [ Disconnect client ]
               else [ ReadAvailable client | Epoll.InEvent `elem` evs ]
                 ++ [ WriteAvailable client | Epoll.OutEvent `elem` evs ]

poll :: EventLoop -> Int -> IO [Event]
poll el@EventLoop{..} timeout = do
    epollEvents <- if timeout == 0 then Epoll.waitNonblock epoll
                                   else Epoll.wait epoll timeout
    fmap concat $ sequence $ map (processEpollEvent el) epollEvents
