{-# LANGUAGE DefaultSignatures, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Graphics.Wayland.Server.Monad
    ( WlState(..)
    , Wayland, runWayland
    , S.get, S.put, S.modify, S.modify', S.gets
    ) where

--import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
--import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad.Trans
import Control.Monad.Writer

import qualified Graphics.Wayland.Server.Monad.State as S

data WlState = WlState
    {
    }

runWayland :: Wayland a -> IO a
runWayland = undefined -- todo

newtype Wayland a = Wayland (S.StateT WlState IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

class Monad m => MonadWayland m where
    liftWl :: Wayland a -> m a
    default liftWl :: (MonadTrans t, MonadWayland m1, m ~ t m1) => Wayland a -> m a
    liftWl = lift . liftWl

instance MonadWayland Wayland where
    liftWl = id

instance MonadWayland m => MonadWayland (ReaderT r m)
instance (MonadWayland m, Monoid w) => MonadWayland (WriterT w m)
instance MonadWayland m => MonadWayland (StateT s m)
instance (MonadWayland m, Monoid w) => MonadWayland (RWST r w s m)
instance MonadWayland m => MonadWayland (ExceptT e m)
instance MonadWayland m => MonadWayland (ContT r m)
