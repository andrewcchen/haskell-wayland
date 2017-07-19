{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#include <wayland-server.h>

module Graphics.Wayland.Server.Surface
    ( WlSurface
    , WlSurfaceInterface
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


{-
void (* destroy ) (struct wl_client *client, struct wl_resource *resource)
void (* attach ) (struct wl_client *client, struct wl_resource *resource, struct wl_resource *buffer, int32_t x, int32_t y)
void (* damage ) (struct wl_client *client, struct wl_resource *resource, int32_t x, int32_t y, int32_t width, int32_t height)
void (* frame ) (struct wl_client *client, struct wl_resource *resource, uint32_t callback)
void (* set_opaque_region ) (struct wl_client *client, struct wl_resource *resource, struct wl_resource *region)
void (* set_input_region ) (struct wl_client *client, struct wl_resource *resource, struct wl_resource *region)
void (* commit ) (struct wl_client *client, struct wl_resource *resource)
void (* set_buffer_transform ) (struct wl_client *client, struct wl_resource *resource, int32_t transform)
void (* set_buffer_scale ) (struct wl_client *client, struct wl_resource *resource, int32_t scale)
void (* damage_buffer ) (struct wl_client *client, struct wl_resource *resource, int32_t x, int32_t y, int32_t width, int32_t height)
-}
data WlSurfaceInterface m = WlSurfaceInterface
    { destroy :: WlClient -> WlResource -> m ()
    , attach :: WlClient -> WlResource -> WlResource -> Int32 -> Int32 -> m ()
    , damage :: WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
    , frame :: WlClient -> WlResource -> Word32 -> m ()
    , setOpaqueRegion :: WlClient -> WlResource -> WlResource -> m ()
    , setInputRegion :: WlClient -> WlResource -> WlResource -> m ()
    , commit :: WlClient -> WlResource -> m ()
    , setBufferTransform :: WlClient -> WlResource -> Int32 -> m ()
    , setBufferScale :: WlClient -> WlResource -> Int32 -> m ()
    , damageBuffer :: WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
    }

foreign import ccall "wrapper" mkWlSurfaceFunc0 :: (WlClient -> WlResource -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc1 :: (WlClient -> WlResource -> WlResource -> Int32 -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> WlResource -> Int32 -> Int32 -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc2 :: (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc3 :: (WlClient -> WlResource -> Word32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Word32 -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc4 :: (WlClient -> WlResource -> WlResource -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> WlResource -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc5 :: (WlClient -> WlResource -> WlResource -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> WlResource -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc6 :: (WlClient -> WlResource -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc7 :: (WlClient -> WlResource -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Int32 -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc8 :: (WlClient -> WlResource -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Int32 -> IO ()))
foreign import ccall "wrapper" mkWlSurfaceFunc9 :: (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()) -> IO (FunPtr (WlClient -> WlResource -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()))

instance MonadWayland s m => InterfaceImplementation (WlSurfaceInterface m) m where
    wrapImplementation i = wrapCallback <$> getStateRef >>= \wcb -> liftIO $ sequence
        [ fmap castFunPtr $ mkWlSurfaceFunc0 $ wcb .: destroy i
        , fmap castFunPtr $ mkWlSurfaceFunc1 $ wcb .:::: attach i
        , fmap castFunPtr $ mkWlSurfaceFunc2 $ wcb .::::: damage i
        , fmap castFunPtr $ mkWlSurfaceFunc3 $ wcb .:: frame i
        , fmap castFunPtr $ mkWlSurfaceFunc4 $ wcb .:: setOpaqueRegion i
        , fmap castFunPtr $ mkWlSurfaceFunc5 $ wcb .:: setInputRegion i
        , fmap castFunPtr $ mkWlSurfaceFunc6 $ wcb .: commit i
        , fmap castFunPtr $ mkWlSurfaceFunc7 $ wcb .:: setBufferTransform i
        , fmap castFunPtr $ mkWlSurfaceFunc8 $ wcb .:: setBufferScale i
        , fmap castFunPtr $ mkWlSurfaceFunc9 $ wcb .::::: damageBuffer i
        ]
