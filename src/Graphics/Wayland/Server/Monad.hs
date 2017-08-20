{-# LANGUAGE DefaultSignatures, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE RankNTypes #-}

module Graphics.Wayland.Server.Monad
    where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import Data.IORef
import Data.Unique
import Data.Hashable
import Data.Dynamic
import qualified Data.IntMap as M

import Graphics.Wayland.Object
import Graphics.Wayland.Registry
import Graphics.Wayland.Server.EventLoop

type Dispatcher m = Int -> Object -> [Dynamic] -> m ()

data Client m = Client
    { clientUnique :: Unique
    , _connection :: ClientConn
    , _registry :: IORef Registry
    , _dispatchers :: IORef (M.IntMap (Interface, Dispatcher m))
    }
makeLenses ''Client

instance Eq (Client m) where
    a == b = clientUnique a == clientUnique b
instance Ord (Client m) where
    a `compare` b = clientUnique a `compare` clientUnique b
instance Hashable (Client m) where
    hash Client{..} = hash clientUnique
    hashWithSalt s Client{..} = s `hashWithSalt` clientUnique
instance Show (Client m) where
    show Client{..} = undefined -- TODO

data WlState = WlState
    { eventLoop :: EventLoop
    , _clients :: Int -- TODO
    }
makeLenses ''WlState

newtype Wayland a = Wayland { runWayland :: ReaderT WlState IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

class MonadIO m => MonadWayland m where
    liftWl :: Wayland a -> m a
    default liftWl :: (MonadTrans t, MonadWayland m1, m ~ t m1) => Wayland a -> m a
    liftWl = lift . liftWl

wlAsks :: MonadWayland m => (WlState -> a) -> m a
wlAsks = liftWl . Wayland . asks

instance MonadWayland Wayland where
    liftWl = id
instance MonadWayland m => MonadWayland (ReaderT r m)
instance (MonadWayland m, Monoid w) => MonadWayland (WriterT w m)
instance MonadWayland m => MonadWayland (StateT s m)
instance (MonadWayland m, Monoid w) => MonadWayland (RWST r w s m)
instance MonadWayland m => MonadWayland (ExceptT e m)
instance MonadWayland m => MonadWayland (ContT r m)
