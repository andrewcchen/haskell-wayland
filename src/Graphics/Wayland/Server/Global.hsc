#include <wayland-server.h>

module Graphics.Wayland.Server.Global
    ( WlGlobal, WlInterface
    , WlGlobalBindFunc, WlGlobalBindFuncIO
    , wlGlobalCreate, wlGlobalDestroy
    , wlCompositorInterface
    ) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types
import Foreign.Ptr

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


-- typedef void(* wl_global_bind_func_t) (struct wl_client *client, void *data, uint32_t version, uint32_t id)
type WlGlobalBindFunc m = WlClient -> #{type uint32_t} -> #{type uint32_t} -> m ()
type WlGlobalBindFuncIO = WlClient -> Ptr () -> #{type uint32_t} -> #{type uint32_t} -> IO ()

foreign import ccall "wrapper"
    mkGlobalBindFunc :: WlGlobalBindFuncIO -> IO (FunPtr WlGlobalBindFuncIO)

foreign import ccall "wl_global_create"
    c_wl_global_create :: WlDisplay -> WlInterface -> #{type int} -> Ptr () -> FunPtr WlGlobalBindFuncIO -> IO WlGlobal
wlGlobalCreate :: MonadWayland s m => WlDisplay -> WlInterface -> #{type int} -> WlGlobalBindFunc m -> m WlGlobal
wlGlobalCreate a b c e = do
    r <- getStateRef
    let ew e1 e2 e3 e4 = wrapCallback r $ e e1 e3 e4
    -- yes I know this is a memory leak, but realistically how many globals are you going to create
    ewp <- liftIO $ mkGlobalBindFunc ew
    wrapCall $ c_wl_global_create a b c nullPtr ewp

foreign import ccall "wl_global_destroy"
    c_wl_global_destroy :: WlGlobal -> IO ()
wlGlobalDestroy :: MonadWayland s m => WlGlobal -> m ()
wlGlobalDestroy = wrapCall . c_wl_global_destroy

foreign import ccall "&wl_compositor_interface"
    wlCompositorInterface :: WlInterface
