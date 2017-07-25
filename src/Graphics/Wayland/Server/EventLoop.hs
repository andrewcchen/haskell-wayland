{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Graphics.Wayland.Server.EventLoop
    ( ClientConn(..)
    , Event(..)
    , EventLoop, newEventLoop, closeEventLoop
    , addSocket, removeSocket
    , addClient, removeClient
    , newSocket, acceptClient
    , poll
    ) where

--import Graphics.Wayland.Internal.Connection
import qualified Graphics.Wayland.Internal.Socket as Sock
-- import qualified Graphics.Wayland.Server.Monad as W

import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Coerce (coerce)
import Data.Dynamic
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Foreign.C.Types (CUInt)
import System.IO
import System.IO.Error (tryIOError, catchIOError)
import qualified Data.HashTable.IO as H

import qualified System.Linux.Epoll.Base as Epoll
import System.Linux.Epoll.Base ((=~))


newtype ListenSocket = ListenSocket Sock.Socket
    deriving (Eq, Ord, Show, Hashable)

data ClientConn = ClientConn
    { clientSocket :: Sock.Socket
    , processId :: CUInt
    , userId :: CUInt
    , groupId :: CUInt
    , processCmdline :: String
    } deriving (Show)

instance Eq ClientConn where
    (==) = (==) `on` clientSocket
instance Ord ClientConn where
    compare = compare `on` clientSocket
instance Hashable ClientConn where
    hashWithSalt s ClientConn{..} = s `hashWithSalt` clientSocket

data EventLoop = EventLoop
    { epoll :: Epoll.Device
    , listenDescs :: H.BasicHashTable ListenSocket (Epoll.Descriptor Dynamic)
    , clientDescs :: H.BasicHashTable ClientConn (Epoll.Descriptor Dynamic)
    }

-- todo fix epoll
newEventLoop :: IO EventLoop
newEventLoop = do
    epoll <- Epoll.create (fromJust $ Epoll.toSize 128)
    listenDescs <- H.new
    clientDescs <- H.new
    return EventLoop{..}

closeEventLoop :: EventLoop -> IO ()
closeEventLoop EventLoop{..} = do
    Epoll.close epoll
    H.mapM_ (\(s,_) -> Sock.close $ coerce s) listenDescs
    H.mapM_ (\(c,_) -> Sock.close $ clientSocket c) clientDescs

addSocket :: EventLoop -> Sock.Socket -> IO ListenSocket
addSocket EventLoop{..} sock = do
    let fd = Sock.fdSocket $ sock
        flags = [Epoll.inEvent]
        sock' = ListenSocket sock
    desc <- Epoll.add epoll (toDyn sock') flags fd
    H.insert listenDescs sock' desc
    return sock'

addClient :: EventLoop -> Sock.Socket -> IO ClientConn
addClient EventLoop{..} sock = do
    let clientSocket = sock
    (processId,userId,groupId) <- Sock.getPeerCred sock
    processCmdline <- getProcessCmdline processId
    let client = ClientConn{..}
        fd = Sock.fdSocket $ sock
        flags = [Epoll.inEvent, Epoll.outEvent]
    desc <- Epoll.add epoll (toDyn client) flags fd
    H.insert clientDescs client desc
    return client

removeSocket :: EventLoop -> ListenSocket -> IO ()
removeSocket EventLoop{..} sock = do
    desc <- fromJust <$> H.lookup listenDescs sock
    Epoll.delete epoll desc
    Sock.close $ coerce sock
    H.delete listenDescs sock

removeClient :: EventLoop -> ClientConn -> IO ()
removeClient EventLoop{..} client = do
    desc <- fromJust <$> H.lookup clientDescs client
    Epoll.delete epoll desc
    Sock.close $ clientSocket client
    H.delete clientDescs client

newSocket :: EventLoop -> String -> IO ListenSocket
newSocket el path = do
    sock <- Sock.socket (Sock.sockStream .|. Sock.sockNonblock .|. Sock.sockCloexec)
    Sock.bind sock path
    addSocket el sock

acceptClient :: EventLoop -> ListenSocket -> IO ClientConn
acceptClient el (ListenSocket sock) = do
    cSock <- Sock.accept sock (Sock.sockNonblock .|. Sock.sockCloexec)
    addClient el cSock

data Event = Connect ClientConn
           | Disconnect ClientConn
           | ReadAvailable ClientConn
           | WriteAvailable ClientConn

processEvent :: EventLoop -> Epoll.Event Dynamic -> IO (Either IOError Event)
processEvent el event
    | dynTypeRep dyn == typeRep (Proxy :: Proxy ListenSocket) = case () of
      _ | eTyp =~ Epoll.inEvent -> tryIOError $
            Connect <$> acceptClient el sock
        | otherwise -> return $ Left $ userError $ show eTyp ++ show sock
    | dynTypeRep dyn == typeRep (Proxy :: Proxy ClientConn) = case () of
      _ | eTyp =~ Epoll.inEvent -> return $ Right $ ReadAvailable client
        | eTyp =~ Epoll.outEvent -> return $ Right $ WriteAvailable client
        | eTyp =~ Epoll.hangupEvent -> tryIOError $
            removeClient el client >> return (Disconnect client)
        | otherwise -> return $ Left $ userError $ show eTyp ++ show client
    | otherwise = error "wtf"
    where dyn = Epoll.eventRef event
          eTyp = Epoll.eventType event
          sock = fromJust $ fromDynamic dyn :: ListenSocket
          client = fromJust $ fromDynamic dyn :: ClientConn

poll :: EventLoop -> Int -> IO [Event]
poll el@EventLoop{..} timeout = do
    evs <- Epoll.wait (fromJust $ Epoll.toDuration timeout) epoll
    (errs, res) <- partitionEithers <$> mapM (processEvent el) evs
    when (length errs > 0) $ hPutStrLn stderr $ intercalate "\n" $ map (("poll: "++).show) errs
    return res

getProcessCmdline :: CUInt -> IO String
getProcessCmdline processId = flip catchIOError (const $ return "") $
    withFile ("/proc/"++show processId++"/cmdline") ReadMode hGetContents
