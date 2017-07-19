{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#include <wayland-server.h>

module Graphics.Wayland.Server.Shm
    ( wlDisplayInitShm
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


foreign import ccall "wl_display_init_shm"
    c_wl_display_init_shm :: WlDisplay -> IO Int32
wlDisplayInitShm :: MonadWayland s m => WlDisplay -> m Int32
wlDisplayInitShm = wrapCall . c_wl_display_init_shm

--TODO implement wl_shm_buffer functions
