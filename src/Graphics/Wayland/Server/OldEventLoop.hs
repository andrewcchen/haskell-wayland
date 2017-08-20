{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Graphics.Wayland.Server.EventLoop
    ( Client(..)
    , Event(..)
    , EventLoop, newEventLoop, closeEventLoop
    , addSocket, removeSocket
    , addClient, removeClient
    , newSocket, acceptClient
    , poll
    ) where

--import Graphics.Wayland.Internal.Connection
import qualified Graphics.Wayland.Internal.Socket as S
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
--import qualified Data.HashTable.IO as H

import qualified System.Linux.Epoll.Base as Epoll
import System.Linux.Epoll.Base ((=~))


newtype ListenSocket = ListenSocket S.Socket
    deriving (Eq, Ord, Show, Hashable)

data Client = Client
    { clientSocket :: S.Socket
    , processId :: CUInt
    , userId :: CUInt
    , groupId :: CUInt
    , processCmdline :: String
    } deriving (Show)

instance Eq Client where
    (==) = (==) `on` clientSocket
instance Ord Client where
    compare = compare `on` clientSocket
instance Hashable Client where
    hashWithSalt s Client{..} = s `hashWithSalt` clientSocket

data EventLoop = EventLoop
    { epoll :: Epoll.Device
    , listenDescs :: H.BasicHashTable ListenSocket (Epoll.Descriptor Dynamic)
    , clientDescs :: H.BasicHashTable Client (Epoll.Descriptor Dynamic)
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
    H.mapM_ (\(s,_) -> S.close $ coerce s) listenDescs
    H.mapM_ (\(c,_) -> S.close $ clientSocket c) clientDescs

addSocket :: EventLoop -> S.Socket -> IO ListenSocket
addSocket EventLoop{..} sock = do
    let fd = S.fdSocket $ sock
        flags = [Epoll.inEvent]
        sock' = ListenSocket sock
    desc <- Epoll.add epoll (toDyn sock') flags fd
    H.insert listenDescs sock' desc
    return sock'

addClient :: EventLoop -> S.Socket -> IO Client
addClient EventLoop{..} sock = do
    let clientSocket = sock
    (processId,userId,groupId) <- S.getPeerCred sock
    processCmdline <- getProcessCmdline processId
    let client = Client{..}
        fd = S.fdSocket $ sock
        flags = [Epoll.inEvent, Epoll.outEvent]
    desc <- Epoll.add epoll (toDyn client) flags fd
    H.insert clientDescs client desc
    return client

removeSocket :: EventLoop -> ListenSocket -> IO ()
removeSocket EventLoop{..} sock = do
    desc <- fromJust <$> H.lookup listenDescs sock
    Epoll.delete epoll desc
    S.close $ coerce sock
    H.delete listenDescs sock

removeClient :: EventLoop -> Client -> IO ()
removeClient EventLoop{..} client = do
    desc <- fromJust <$> H.lookup clientDescs client
    Epoll.delete epoll desc
    S.close $ clientSocket client
    H.delete clientDescs client

newSocket :: EventLoop -> String -> IO ListenSocket
newSocket el path = do
    sock <- S.socket (S.sockStream .|. S.sockNonblock .|. S.sockCloexec)
    S.bind sock path
    addSocket el sock

acceptClient :: EventLoop -> ListenSocket -> IO Client
acceptClient el (ListenSocket sock) = do
    cSock <- S.accept sock (S.sockNonblock .|. S.sockCloexec)
    addClient el cSock

data Event = Connect Client
           | Disconnect Client
           | ReadAvailable Client
           | WriteAvailable Client

processEvent :: EventLoop -> Epoll.Event Dynamic -> IO (Either IOError Event)
processEvent el event
    | dynTypeRep dyn == typeRep (Proxy :: Proxy ListenSocket) = case () of
      _ | eTyp =~ Epoll.inEvent -> tryIOError $
            Connect <$> acceptClient el sock
        | otherwise -> return $ Left $ userError $ show eTyp ++ show sock
    | dynTypeRep dyn == typeRep (Proxy :: Proxy Client) = case () of
      _ | eTyp =~ Epoll.inEvent -> return $ Right $ ReadAvailable client
        | eTyp =~ Epoll.outEvent -> return $ Right $ WriteAvailable client
        | eTyp =~ Epoll.hangupEvent -> tryIOError $
            removeClient el client >> return (Disconnect client)
        | otherwise -> return $ Left $ userError $ show eTyp ++ show client
    | otherwise = error "wtf"
    where dyn = Epoll.eventRef event
          eTyp = Epoll.eventType event
          sock = fromJust $ fromDynamic dyn :: ListenSocket
          client = fromJust $ fromDynamic dyn :: Client

poll :: EventLoop -> Int -> IO [Event]
poll el@EventLoop{..} timeout = do
    evs <- Epoll.wait (fromJust $ Epoll.toDuration timeout) epoll
    (errs, res) <- partitionEithers <$> mapM (processEvent el) evs
    when (length errs > 0) $ hPutStrLn stderr $ intercalate "\n" $ map (("poll: "++).show) errs
    return res

getProcessCmdline :: CUInt -> IO String
getProcessCmdline processId = -- TODO use non-lazy IO
    withFile ("/proc/"++show processId++"/cmdline") ReadMode hGetContents
        `catchIOError` (const (return ""))
