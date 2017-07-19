#include <wayland-server.h>

module Graphics.Wayland.Server.Resource
    ( WlResource
    , wlResourceCreate
    , wlResourceSetImplementation
    , wlResourceGetVersion
    , wlResourcePostNoMemory
    ) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


foreign import ccall "wl_resource_create"
    c_wl_resource_create :: WlClient -> WlInterface -> Int32 -> Int32 -> IO WlResource
wlResourceCreate :: MonadWayland s m
                 => WlClient -> WlInterface -> Int32 -> Int32 -> m WlResource
wlResourceCreate = wrapCall .::: c_wl_resource_create -- XXX is wrapCall really necessary?

foreign import ccall "wrapper"
    mkWlResourceDestroyFuncPtr :: (WlResource -> IO ()) -> IO (FunPtr (WlResource -> IO ()))

foreign import ccall unsafe "wl_resource_set_implementation"
    c_wl_resource_set_implementation :: WlResource -> Ptr () -> Ptr () -> FunPtr (WlResource -> IO ()) -> IO ()
wlResourceSetImplementation :: (MonadWayland s m, InterfaceImplementation i m)
                            => WlResource -> i -> (WlResource -> m ()) -> m ()
wlResourceSetImplementation res imp userDestroy = do
    -- this function is so long because of the need to free the pointers
    ref <- getStateRef
    destroyw <- liftIO $ mkWlResourceDestroyFuncPtr $ wrapCallback ref . destroy
    impw <- wrapImplementation imp
    arrayPtr <- liftIO $ mallocArray $ #{size void*} * length impw
    liftIO $ pokeArray arrayPtr impw
    ptrsPtr <- liftIO $ fmap castStablePtrToPtr $ newStablePtr ([arrayPtr], impw)
    liftIO $ c_wl_resource_set_implementation res (castPtr arrayPtr) ptrsPtr destroyw
    where destroy x = (liftIO $ wlResourceGetUserData x >>= freePointers) >> userDestroy x

foreign import ccall unsafe "wl_resource_get_user_data"
    wlResourceGetUserData :: WlResource -> IO (Ptr ())

foreign import ccall unsafe "wl_resource_get_version"
    c_wl_resource_get_version :: WlResource -> IO Int32
wlResourceGetVersion :: MonadIO m => WlResource -> m Int32
wlResourceGetVersion = liftIO . c_wl_resource_get_version

foreign import ccall unsafe "wl_resource_post_no_memory"
    c_wl_resource_post_no_memory :: WlResource -> IO ()
wlResourcePostNoMemory :: MonadIO m => WlResource -> m ()
wlResourcePostNoMemory = liftIO . c_wl_resource_post_no_memory
