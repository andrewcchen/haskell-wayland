module Graphics.Wayland.Client where

import Graphics.Wayland.Object
import Graphics.Wayland.Registry
import Graphics.Wayland.Connection

import Data.Unique
import Data.IORef

data Client = Client
    { clientUnique :: Unique
    , clientRegistry :: IORef Registry
    }

instance Eq Client where
    (==) = (==) `on:wq
