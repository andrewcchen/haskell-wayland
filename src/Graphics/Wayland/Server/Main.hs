{-# LANGUAGE AllowAmbiguousTypes, DefaultSignatures, RankNTypes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Wayland.Server.Main
    (
    ) where

import Graphics.Wayland.Internal.Connection
import Graphics.Wayland.Server.EventLoop
import Graphics.Wayland.Internal.Registry
import Graphics.Wayland.Internal.Marshal
--import Graphics.Wayland.Server.Monad
--import Graphics.Wayland.Server.Monad.State as W

import Data.Maybe
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.RWS hiding (ask, asks)
import Control.Monad.Reader hiding (ask, asks)
import Control.Monad.State
import Control.Monad.Writer
import Foreign.C.Error
import System.IO.Error
import qualified Data.HashTable.IO as H
import Data.Dynamic
import Data.IORef


-- do something


















--class MonadIO m => MonadWayland m where
--    liftWl :: Wayland m a -> m a
--    default liftWl :: (MonadTrans t, MonadWayland m1, m ~ t m1) => Wayland a -> m a
--    liftWl = lift . liftWl
--
--newtype Wayland m a = Wayland { runWl :: WlState m -> IO a }
--
--instance Functor (Wayland m) where
--    fmap f m = Wayland $ \s -> fmap f $ runWl m s
--    {-# INLINE fmap #-}
--
--instance Applicative (Wayland m) where
--    pure = return
--    {-# INLINE pure #-}
--    (<*>) = ap
--    {-# INLINE (<*>) #-}
--
--instance Monad (Wayland m) where
--    return a = Wayland $ \_ -> return a
--    {-# INLINE return #-}
--    m >>= k = Wayland $ \s -> do
--        a <- runWl m s
--        runWl (k a) s
--    {-# INLINE (>>=) #-}
--
--instance MonadIO (Wayland m) where
--    liftIO x = Wayland $ \_ -> x
--    {-# INLINE liftIO #-}
--
--instance MonadWayland (Wayland m) where
--    liftWl = id
--    {-# INLINE liftWl #-}
--
--instance MonadWayland m => MonadWayland (ReaderT r m)
--instance (MonadWayland m, Monoid w) => MonadWayland (WriterT w m)
--instance MonadWayland m => MonadWayland (StateT s m)
--instance (MonadWayland m, Monoid w) => MonadWayland (RWST r w s m)
--instance MonadWayland m => MonadWayland (ExceptT e m)
--instance MonadWayland m => MonadWayland (ContT r m)
--
--ask :: forall m a. MonadWayland m => m (WlState m)
--ask = liftWl $ Wayland $ \s -> return s
--
--asks :: forall m a. MonadWayland m => (WlState m -> a) -> m a
--asks f = f <$> ask
--
--data WlState m = WlState
--    { eventLoop :: EventLoop
--    , welp :: Client
--    }
--makeLenses ''WlState
--
--runWayland :: Wayland a -> IO a
--runWayland = undefined -- todo
--
--data Client = Client
--    { clientConn :: ClientConn
--    , clientRegistry :: Registry
--    --, client
--    }
--
--stuff a b = getObj a b :: forall o. Object o => o
--
--readMessage :: ClientConn -> IORef Registry -> IO (Either String Message)
--readMessage client reg = do
--    let sock = clientSocket client
--    (sender, opcode, len) <- maybe (error "welp") id <$> recvMessageHeader sock
--    body <- recvMessageBody sock len
--    obj <- stuff reg sender -- getObj reg sender
--    let spec = objSpec obj
    --when (opcode >= len) $ (ioError "invalid opcode: "++show opcode) -- todo
    --let argSpecs =  !! opcode
    --fds <- recvMessageFds sock argSpecs
    --args <- either (liftIO . ioError) return $ decodeArgs argSpecs body fds
--    undefined


--processEvent :: MonadWayland m => Event -> m (Interface m, Message)
--processEvent event = do
--    ifs <- (asks $ clientInterfaces . welp :: m (H.BasicHashTable WlId (Interface m)))
--    kek <- liftIO $ H.lookup ifs (WlId 1)
--    let kek' = fromJust kek
--    let lol = interfaceImpl kek'
--    lol undefined
--    undefined


--readMessage :: Client -> Message


--readClient :: MonadWayland m => Client -> m ()
--readClient client = do
--    let sock = clientSocket client
--    ret <- runMaybeT $ do
--        (sender, opcode, len) <- MaybeT $ recvMessageHeader sock
--        body <- recvMessageBody sock len
--        maybeId <- gets _intIdMap >>= \m -> liftIO $ idGetInt m sender
--        when (isNothing maybeId) $ liftIO $ ioError "recieved invalid objectid" -- todo handle error
--        obj <- gets _objReg >>= \r -> regGet r $ fromJust maybeId
--        let funcSpecs = objInterface obj ^. interfaceFuncs
--        when (opcode >= len) $ liftIO $ ioError "recieved invalid opcode" -- todo
--        let argSpecs = funcSpecs !! opcode
--        fds <- recvMessageFds sock argSpecs
--        args <- either (liftIO . ioError) return $ decodeArgs argSpecs body fds
--    undefined
--
--pollAndDispatch :: MonadWayland m => m ()
--pollAndDispatch = do
--    eventLoop <- W.gets $ view eventLoop
--    events <- liftIO $ poll eventLoop 20
--    undefined
