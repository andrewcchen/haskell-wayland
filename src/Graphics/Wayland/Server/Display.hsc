#include <wayland-server.h>

module Graphics.Wayland.Server.Display
    ( WlDisplay, WlEventLoop
    , wlDisplayCreate, wlDisplayDestroy
    , wlDisplayGetEventLoop
    ) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


foreign import ccall "wl_display_create"
    c_wl_display_create :: IO WlDisplay
wlDisplayCreate :: MonadWayland s m => m WlDisplay
wlDisplayCreate = wrapCall c_wl_display_create

foreign import ccall "wl_display_destroy"
    c_wl_display_destroy :: WlDisplay -> IO ()
wlDisplayDestroy :: MonadWayland s m => WlDisplay -> m ()
wlDisplayDestroy = wrapCall . c_wl_display_destroy

foreign import ccall "wl_display_get_event_loop"
    c_wl_display_get_event_loop :: WlDisplay -> IO WlEventLoop
wlDisplayGetEventLoop :: MonadWayland s m => WlDisplay -> m WlEventLoop
wlDisplayGetEventLoop = wrapCall . c_wl_display_get_event_loop
