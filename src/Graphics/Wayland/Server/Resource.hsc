#include <wayland-server.h>

module Graphics.Wayland.Server.Resource
    ( WlResource
    , wlResourceCreate
    , wlResourceGetClient
    -- , wlResourceGetVersion
    , wlResourcePostNoMemory
    ) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Data

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


wlResourceGetData :: WlResource -> IO (Ptr a)
wlResourceGetData (WlResource ptr) = #{peek struct wl_resource, data} ptr

wlResourceGetClient :: WlResource -> IO WlClient
wlResourceGetClient (WlResource ptr) = WlClient <$> #{peek struct wl_resource, client} ptr

foreign import ccall unsafe "wl_resource_create"
    wl_resource_create :: WlClient -> WlInterface -> Int32 -> Word32 -> IO WlResource

type WlDispatcherFun = Ptr () -> WlResource -> Word32 -> Ptr () -> Ptr () -> IO Int32
foreign import ccall "wrapper"
    mkDispatcherPtr :: WlDispatcherFun -> IO (FunPtr WlDispatcherFun)

type DestroyFun = WlResource -> IO ()
foreign import ccall "wrapper"
    mkDestroyPtr :: DestroyFun -> IO (FunPtr DestroyFun)

foreign import ccall unsafe "wl_resource_set_dispatcher"
    wl_resource_set_dispatcher :: WlResource -> FunPtr WlDispatcherFun -> Ptr () -> Ptr () -> FunPtr DestroyFun -> IO ()

destroy :: WlResource -> IO ()
destroy r = do
    ptr <- wlResourceGetData r
    mapM_ (\i -> freeHaskellFunPtr =<< peekElemOff ptr i) [0..1]
    free ptr

wlResourceCreate :: WaylandInterface i => WlClient -> i -> Int32 -> Word32 -> IO WlResource
wlResourceCreate client interface version id = do
    resource <- wl_resource_create client (wlInterface interface) version id
    dispatcherPtr <- mkDispatcherPtr (\_ r o _ a -> wlDispatcher interface r o a >> return 0)
    destroyPtr <- mkDestroyPtr destroy
    ptrPtr <- mallocArray 2 :: IO (Ptr (FunPtr ()))
    pokeByteOff ptrPtr 0 dispatcherPtr
    pokeByteOff ptrPtr #{size void*} destroyPtr
    wl_resource_set_dispatcher resource dispatcherPtr nullPtr (castPtr ptrPtr) destroyPtr
    return resource



foreign import ccall unsafe "wl_resource_post_no_memory"
    c_wl_resource_post_no_memory :: WlResource -> IO ()
wlResourcePostNoMemory :: MonadIO m => WlResource -> m ()
wlResourcePostNoMemory = liftIO . c_wl_resource_post_no_memory

{-
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
-}

--foreign import ccall unsafe "wl_resource_get_user_data"
--    wlResourceGetUserData :: WlResource -> IO (Ptr ())
--
--foreign import ccall unsafe "wl_resource_get_version"
--    c_wl_resource_get_version :: WlResource -> IO Int32
--wlResourceGetVersion :: MonadIO m => WlResource -> m Int32
--wlResourceGetVersion = liftIO . c_wl_resource_get_version
