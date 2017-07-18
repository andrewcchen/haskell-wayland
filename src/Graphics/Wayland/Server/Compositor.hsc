{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#include <wayland-server.h>

module Graphics.Wayland.Server.Compositor
    ( WlCompositor
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Graphics.Wayland.Server.Internal
import Graphics.Wayland.Server.Types


data WlClientInterface m = WlClientInterface
    { createSurface :: WlClient -> WlResource -> #{type uint32_t} -> m ()
    , createRegion :: WlClient -> WlResource -> #{type uint32_t} -> m ()
    }

type InterfaceFunc m = WlClient -> WlResource -> #{type uint32_t} -> m ()
type InterfaceFuncIO = WlClient -> WlResource -> #{type uint32_t} -> IO ()

foreign import ccall "wrapper"
    mkInterfaceFuncPtr :: InterfaceFuncIO -> IO (FunPtr InterfaceFuncIO)

instance MonadWayland s m => InterfaceImplementation (WlClientInterface m) m where
    wrapImplementation i = do
        let f1 x1 x2 x3 = createSurface i x1 x2 x3
            f2 x1 x2 x3 = createRegion i x1 x2 x3
        ref <- getStateRef
        liftIO $ mapM (fmap castFunPtr
                     . mkInterfaceFuncPtr
                     . (wrapCallback ref .::)) [f1, f2]
