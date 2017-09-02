{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Wayland.Server.Client where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Data.Function
import Data.Hashable
import Data.Unique
import System.IO
import System.IO.Error
import Text.Printf
import Control.Concurrent.MVar
import Data.Typeable
import Data.Coerce

import Graphics.Wayland.Object
import Graphics.Wayland.Connection
import Graphics.Wayland.Registry
import Graphics.Wayland.System.Socket

data Client = Client
    { clientUnique :: Unique
    , clientSocket :: Socket
    , clientInfo :: ProcessInfo
    , clientRegistry :: MVar Registry
    }

data ProcessInfo = ProcessInfo
    { processId :: Int
    , processUserId :: Int
    , processGroupId :: Int
    , processCmd :: String
    }

instance Show Client where
    show Client{clientInfo = ProcessInfo{..}} =
        printf "Client {pid = %d, uid = %d, gid = %d, cmd = %s}"
            processId processUserId processGroupId processCmd
instance Eq Client where
    (==) = (==) `on` clientUnique
    {-# INLINE (==) #-}
instance Ord Client where
    compare = compare `on` clientUnique
    {-# INLINE compare #-}
instance Hashable Client where
    hash = hash . clientUnique
    {-# INLINE hash #-}
    hashWithSalt s x = s `hashWithSalt` clientUnique x
    {-# INLINE hashWithSalt #-}
instance Connection Client where
    connRecv c = recv (clientSocket c) 0
    connSend c = send (clientSocket c) 0
    connRecvFd c = recvFdCloexec (clientSocket c)
    connSendFd c = sendFd (clientSocket c)
    {-# INLINE connRecv #-}
    {-# INLINE connSend #-}
    {-# INLINE connRecvFd #-}
    {-# INLINE connSendFd #-}

newClient :: Socket -> IO Client
newClient clientSocket = do
    clientUnique <- newUnique
    (processId, processUserId, processGroupId) <-
        over each fromIntegral <$> getPeerCred clientSocket
    processCmd <- getProcessCmd processId
    let clientInfo = ProcessInfo{..}
    clientRegistry <- newMVar $ newRegistry ServerRegistry
    return Client{..}

getProcessCmd :: Int -> IO String
getProcessCmd pid = (`catchIOError` (return . show)) $
    withFile ("/proc/"++show pid++"/cmdline") ReadMode $ \h ->
        evaluate . force =<< hGetContents h

newObj :: (Typeable a, Coercible Object a) => Client -> IO a
newObj client = modifyMVar (clientRegistry client) regNewObj

insObj :: (Typeable a, Coercible Object a) => Client -> a -> IO ()
insObj client obj = modifyMVar_ (clientRegistry client) $ regIns (coerce obj)

getObj :: (Typeable a, Coercible Object a) => Client -> Int -> IO a
getObj client i = readMVar (clientRegistry client) >>= regGet i
