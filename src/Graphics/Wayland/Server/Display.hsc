#include <wayland-server.h>

module Graphics.Wayland.Server.Display
    ( WlDisplay, WlEventLoop
    , wlDisplayCreate, wlDisplayDestroy
    , wlDisplayRun
    , wlDisplayGetEventLoop
    , wlDisplayAddSocket
    ) where

import Control.Exception.Base
import Control.Monad.IO.Class
import Foreign
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr

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

foreign import ccall "wl_display_run"
    c_wl_display_run :: WlDisplay -> IO ()
wlDisplayRun :: MonadWayland s m => WlDisplay -> m ()
wlDisplayRun = wrapCall . c_wl_display_run

foreign import ccall "wl_display_get_event_loop"
    c_wl_display_get_event_loop :: WlDisplay -> IO WlEventLoop
wlDisplayGetEventLoop :: MonadWayland s m => WlDisplay -> m WlEventLoop
wlDisplayGetEventLoop = wrapCall . c_wl_display_get_event_loop

foreign import ccall unsafe "wl_display_add_socket"
    c_wl_display_add_socket :: WlDisplay -> CString -> IO Int32
wlDisplayAddSocket :: MonadWayland s m => WlDisplay -> Maybe String -> m Int32
wlDisplayAddSocket a Nothing = liftIO $ c_wl_display_add_socket a nullPtr
wlDisplayAddSocket a (Just s) =
    liftIO $ bracket (newCString s) free $ c_wl_display_add_socket a
