{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#include <wayland-server.h>

module Graphics.Wayland.Server.Region
    ( WlRegion
    , WlRegionInterface
    ) where

import Prelude hiding (add, subtract)

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


{-
void (* destroy ) (struct wl_client *client, struct wl_resource *resource)
void (* add ) (struct wl_client *client, struct wl_resource *resource, int32_t x, int32_t y, int32_t width, int32_t height)
void (* subtract ) (struct wl_client *client, struct wl_resource *resource, int32_t x, int32_t y, int32_t width, int32_t height)
-}
data WlRegionInterface m = WlRegionInterface
    { destroy :: WlClient -> WlResource -> m ()
    , add :: WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
    , subtract :: WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
    }

foreign import ccall "wrapper" mkWlRegionFunc0 :: (WlClient -> WlResource -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> IO ()))
foreign import ccall "wrapper" mkWlRegionFunc1 :: (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()))
foreign import ccall "wrapper" mkWlRegionFunc2 :: (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()))

instance MonadWayland s m => InterfaceImplementation (WlRegionInterface m) m where
    wrapImplementation i = wrapCallback <$> getStateRef >>= \wcb -> liftIO $ sequence
        [ fmap castFunPtr $ mkWlRegionFunc0 $ wcb .: destroy i
        , fmap castFunPtr $ mkWlRegionFunc1 $ wcb .::::: add i
        , fmap castFunPtr $ mkWlRegionFunc2 $ wcb .::::: subtract i
        ]
