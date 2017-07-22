{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Graphics.Wayland.Server.EventLoop
    ( WlClient(..)
    , EventLoop, newEventLoop, closeEventLoop
    , addSocket, removeSocket
    , addClient, removeClient
    ) where

import Graphics.Wayland.Internal.Connection
import qualified Graphics.Wayland.Server.Monad as W

import Control.Lens
import Control.Monad.State
import Data.Dynamic
import Data.Function
import Data.Maybe
import Foreign.C.Types
import Network.Socket
import System.IO
import System.IO.Error
import System.Posix.Types
import qualified Data.Map.Strict as M
import qualified System.Linux.Epoll.Base as Epoll

newtype ListenSocket = ListenSocket Socket
    deriving (Eq, Ord, Show)

instance Ord Socket where
    compare = compare `on` fdSocket

data WlClient = WlClient
    { clientSocket :: Socket
    , processId :: CUInt
    , userId :: CUInt
    , groupId :: CUInt
    , processCmdline :: String
    } deriving (Eq, Ord, Show)

data EventLoop = EventLoop
    { _epoll :: Epoll.Device
    , _listenDesc :: M.Map ListenSocket (Epoll.Descriptor Dynamic)
    , _clientDesc :: M.Map WlClient (Epoll.Descriptor Dynamic)
    }
makeLenses ''EventLoop

newEventLoop :: stuff -> IO EventLoop
newEventLoop = undefined

closeEventLoop :: EventLoop -> IO ()
closeEventLoop = undefined

addSocket :: Socket -> EventLoop -> IO (ListenSocket, EventLoop)
addSocket sock el = do
    let fd = Fd $ fdSocket $ sock
        flags = [Epoll.inEvent]
        sock' = ListenSocket sock
    desc <- Epoll.add (el ^. epoll) (toDyn sock') flags fd
    return $ (sock', over listenDesc (M.insert sock' desc) el)

addClient :: Socket -> EventLoop -> IO (WlClient, EventLoop)
addClient sock el = flip runStateT el $ do
    let clientSocket = sock
    (processId,userId,groupId) <- liftIO $ getPeerCred sock
    processCmdline <- liftIO $ getProcessCmdline processId
    let client = WlClient{..}
        fd = Fd $ fdSocket $ sock
        flags = [Epoll.inEvent, Epoll.outEvent]
    ep <- gets $ view epoll
    desc <- liftIO (Epoll.add ep (toDyn client) flags fd)
    modify' $ over clientDesc $ M.insert client desc
    return client

removeSocket :: ListenSocket -> EventLoop -> IO EventLoop
removeSocket sock el = do
    let desc = (el ^. listenDesc) M.! sock
    Epoll.delete (el ^. epoll) desc
    return $ over listenDesc (M.delete sock) el

removeClient :: WlClient -> EventLoop -> IO EventLoop
removeClient client el = do
    let desc = (el ^. clientDesc) M.! client
    Epoll.delete (el ^. epoll) desc
    return $ over clientDesc (M.delete client) el

-- we need to return (clients connects, client disconnects, messages)
-- also we should really be calling epoll_pwait and masking SIGVTALRM
dispatch :: W.MonadWayland m => Int -> EventLoop -> m (([WlClient], [Message]), EventLoop)
dispatch timeout el = flip runStateT el $ do
    let timeout' = fromJust $ Epoll.toDuration timeout
    events <- liftIO $ Epoll.wait timeout' (el ^. epoll)
    --fmap partitionEithers $ forM events $ \ev -> do
    -- StateT EventLoop m [Either WlClient Message]
    undefined

getProcessCmdline :: CUInt -> IO String
getProcessCmdline processId = flip catchIOError (const $ return "") $
    withFile ("/proc/"++show processId++"/cmdline") ReadMode hGetContents
