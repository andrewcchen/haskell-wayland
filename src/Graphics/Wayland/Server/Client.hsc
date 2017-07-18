#include <wayland-server.h>

module Graphics.Wayland.Server.Client
    ( WlClient
    ) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types
