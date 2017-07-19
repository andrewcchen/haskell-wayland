{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#include <wayland-server.h>

module Graphics.Wayland.Server.Compositor
    ( WlCompositor
    , WlCompositorInterface(..)
    , wlCompositorInterface
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


foreign import ccall "&wl_compositor_interface"
    wlCompositorInterface :: WlInterface

{-
void (* create_surface ) (struct wl_client *client, struct wl_resource *resource, uint32_t id)
void (* create_region ) (struct wl_client *client, struct wl_resource *resource, uint32_t id)
-}
data WlCompositorInterface m = WlCompositorInterface
    { createSurface :: WlClient -> WlResource -> Word32 -> m ()
    , createRegion :: WlClient -> WlResource -> Word32 -> m ()
    }

foreign import ccall "wrapper" mkWlCompositorFunc0 :: (WlClient -> WlResource -> Word32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Word32 -> IO ()))
foreign import ccall "wrapper" mkWlCompositorFunc1 :: (WlClient -> WlResource -> Word32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Word32 -> IO ()))

instance MonadWayland s m => InterfaceImplementation (WlCompositorInterface m) m where
    wrapImplementation i = wrapCallback <$> getStateRef >>= \wcb -> liftIO $ sequence
        [ fmap castFunPtr $ mkWlCompositorFunc0 $ wcb .:: createSurface i
        , fmap castFunPtr $ mkWlCompositorFunc1 $ wcb .:: createRegion i
        ]
