{-# LANGUAGE DefaultSignatures, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE RankNTypes #-}

module Graphics.Wayland.Server.Monad
    where

--import Control.Monad.Cont
--import Control.Monad.Except
--import Control.Monad.RWS
import Control.Monad.Reader
--import Control.Monad.State
--import Control.Monad.Writer
import Control.Lens
import Data.IORef
import Data.Unique
import Data.Hashable
import Data.Dynamic
import qualified Data.IntMap as M

import Graphics.Wayland.Object
import Graphics.Wayland.Registry
import Graphics.Wayland.Server.EventLoop
import Graphics.Wayland.Server.Connection

data Client m = Client
    { connection :: ClientConn
    , registry :: IORef Registry
    , dispatchers :: IORef (M.IntMap (Interface, Dispatcher m))
    }

instance Show (Client m) where
    show c = show (connection c)
instance Eq (Client m) where
    (==) = (==) `on` connection
    {-# INLINE (==) #-}
instance Ord (Client m) where
    compare = compare `on` connection
    {-# INLINE compare #-}
instance Hashable (Client m) where
    hashWithSalt s x = s `hashWithSalt` connection x
    {-# INLINE hashWithSalt #-}

data WlState m = WlState
    { eventLoop :: EventLoop
    , clients :: IORef (M.IntMap (Client m))
    }
--makeLenses ''WlState

newtype Wayland m a = Wayland { runWayland :: ReaderT (WlState m) IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

class MonadIO m => MonadWayland m where
    liftWl :: Wayland m a -> m a

newtype Wayland' a = Wayland' (Wayland Wayland' a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadWayland Wayland' where
    liftWl m = Wayland' m

wlAsks :: MonadWayland m => (WlState m -> a) -> m a
wlAsks = liftWl . Wayland . asks
