module Main where

import Control.Monad.State
import Control.Monad.Error

type M a = ErrorT (StateT Identity) a
