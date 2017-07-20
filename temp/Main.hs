{-# LANGUAGE FlexibleContexts #-}

module Main where

--import qualified WMonad.Main as W

import Data.Maybe
--import Control.Concurrent
import System.Posix.Signals

import Graphics.Wayland.Server.Display
import Graphics.Wayland.Server.Shm
import Graphics.Wayland.Server.Compositor
import Graphics.Wayland.Server.Global
import Graphics.Wayland.Server.Resource
import Graphics.Wayland.Server.Shell

compositorImp :: WlCompositorInterface IO
compositorImp = WlCompositorInterface
              { createSurface = \_ _ _ -> return ()
              , createRegion = \_ _ _ -> return ()
              }

shellImp :: WlShellInterface IO
shellImp = WlShellInterface
         { getShellSurface = \_ _ _ _-> return () -- todo
         }

main = do
    disp <- wlDisplayCreate
    putStrLn "hi"
    wlDisplayAddSocket disp Nothing

    wlDisplayInitShm disp

    let ignore = const $ return ()

    let bindcomp client ver id = do
            putStrLn "compositor bind"
            res <- wlResourceCreate client wlCompositorInterface ver id
            wlResourceSetImplementation res compositorImp ignore
    wlGlobalCreate disp wlCompositorInterface 3 bindcomp

    let bindshell client ver id = do
            putStrLn "shell bind"
            res <- wlResourceCreate client wlShellInterface ver id
            wlResourceSetImplementation res shellImp ignore
    wlGlobalCreate disp wlShellInterface 1 bindshell

    putStrLn "running ..."
    wlDisplayRun disp

    wlDisplayDestroy disp
