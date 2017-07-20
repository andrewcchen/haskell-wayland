{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.Wayland.Server.Types where

import Data.Data
import Foreign.Ptr

newtype WlDisplay = WlDisplay (Ptr WlDisplay) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlEventLoop = WlEventLoop (Ptr WlEventLoop) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlGlobal = WlGlobal (Ptr WlGlobal) deriving (Eq, Ord, Show, Typeable, Data)

--newtype WlShm = WlShm (Ptr WlShm) deriving (Eq, Ord, Show, Typeable, Data)

--newtype WlShmPool = WlShmPool (Ptr WlShmPool) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlShmBuffer = WlShmBuffer (Ptr WlShmBuffer) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlCompositor = WlCompositor (Ptr WlCompositor) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlSurface = WlSurface (Ptr WlSurface) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlRegion = WlRegion (Ptr WlRegion) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlShell = WlShell (Ptr WlShell) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlShellSurface = WlShellSurface (Ptr WlShellSurface) deriving (Eq, Ord, Show, Typeable, Data)
