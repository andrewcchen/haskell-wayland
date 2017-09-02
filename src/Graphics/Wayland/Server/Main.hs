--{-# LANGUAGE AllowAmbiguousTypes, DefaultSignatures, RankNTypes, TemplateHaskell, TypeFamilies #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Wayland.Server.Main
    (
    ) where

import Graphics.Wayland.Connection
import Graphics.Wayland.Registry
import Graphics.Wayland.Marshal
import Graphics.Wayland.Server.EventLoop
import Graphics.Wayland.Server.Monad



runEventLoop :: MonadWayland m => m ()
runEventLoop = undefined
