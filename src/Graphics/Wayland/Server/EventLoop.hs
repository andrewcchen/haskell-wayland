{-# LANGUAGE RecordWildCards #-}

module Graphics.Wayland.Server.EventLoop
    where

import Data.IORef
import Foreign.C.Types
import Data.Bits
import Control.Exception.Base

import qualified Data.IntMap as M
import qualified Data.IntSet as S

import Graphics.Wayland.Internal.Socket (Socket(Socket))
import qualified Graphics.Wayland.Internal.Socket as Sock
import qualified Graphics.Wayland.System.Epoll as Epoll

data EventLoop = EventLoop
    { epoll :: Epoll.Device
    , listenSocks :: IORef S.IntSet
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
    listenSocks <- newIORef S.empty
    clientSocks <- newIORef M.empty
    return EventLoop{..}

closeEventLoop :: EventLoop -> IO ()
closeEventLoop EventLoop{..} = return () -- TODO

addListenSocket :: EventLoop -> Socket -> IO ()
addListenSocket EventLoop{..} (Socket fd) = do
    Epoll.add epoll [Epoll.InEvent] fd
    modifyIORef listenSocks (S.insert (fromIntegral fd))

removeListenSocekt :: EventLoop -> Socket -> IO ()
removeListenSocekt EventLoop{..} (Socket fd) = do
    Epoll.delete epoll fd
    modifyIORef listenSocks (S.delete (fromIntegral fd))

addClientSocket :: EventLoop -> Socket -> IO ClientConn
addClientSocket EventLoop{..} clientSocket@(Socket fd) = do
    (processId, userId, groupId) <- Sock.getPeerCred clientSocket
    Epoll.add epoll [Epoll.InEvent, Epoll.OutEvent] fd
    let clientConn = ClientConn{..}
    modifyIORef clientSocks (M.insert (fromIntegral fd) clientConn)
    return clientConn

removeClientConn :: EventLoop -> ClientConn -> IO ()
removeClientConn EventLoop{..} ClientConn{clientSocket = Socket fd} = do
    Epoll.delete epoll fd
    modifyIORef clientSocks (M.delete (fromIntegral fd))

newListenSocket :: EventLoop -> String -> IO Socket
newListenSocket el path = do
    sock <- Sock.socket (Sock.sockStream .|. Sock.sockNonblock .|. Sock.sockCloexec)
    (do Sock.bind sock path
        addListenSocket el sock)
        `onException` Sock.close sock
    return sock

acceptClient :: EventLoop -> Socket -> IO ClientConn
acceptClient el sock = do
    clientSock <- Sock.accept sock (Sock.sockNonblock .|. Sock.sockCloexec)
    addClientSocket el clientSock
