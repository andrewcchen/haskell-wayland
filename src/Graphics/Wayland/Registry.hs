--{-# LANGUAGE KindSignatures, RankNTypes, RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Wayland.Registry where
-- TODO export list

--import Graphics.Wayland.Marshal
import Graphics.Wayland.Object

import Control.Exception
import Data.Coerce
import Control.Monad
import Data.Dynamic
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.IntMap.Lazy as M

data Registry = Registry
    { nextId :: Int
    , freeIds :: [Int]
    , objs :: M.IntMap Object
    }

data RegistryError = ObjectDoesNotExist Int
                   | ObjectAlreadyExists Int
    deriving (Show)
instance Exception RegistryError

data RegistryType = ServerRegistry
                  | ClientRegistry
    deriving (Show)

newRegistry :: RegistryType -> Registry
newRegistry ServerRegistry = Registry { nextId = 0xFFFFFF00
                                      , freeIds = []
                                      , objs = M.empty }
newRegistry ClientRegistry = Registry { nextId = 0x00000001
                                      , freeIds = []
                                      , objs = M.empty }


regGet :: MonadThrow m => Int -> Registry -> m Object
regGet i r =
    maybe (throwM (ObjectDoesNotExist i)) return $ M.lookup i $ objs r

regDel :: MonadThrow m => Object -> Registry -> m Registry
regDel obj r = do
    let i = objId obj
    unless (M.member i (objs r)) $ throwM (ObjectDoesNotExist i)
    return r { freeIds = i : freeIds r
             , objs = M.delete i (objs r)
             }

regIns :: MonadThrow m => Object -> Registry -> m Registry
regIns obj r = do
    let i = objId obj
    when (M.member i (objs r)) $ throwM (ObjectAlreadyExists i)
    return r { objs = M.insert i obj (objs r) }

regNew :: (Typeable a, Coercible Object a, MonadIO m)
       => Registry -> m (a, Registry)
regNew r = do
    let (i, r') = if null (freeIds r)
                     then (nextId r, r { nextId = nextId r + 1 })
                     else (head (freeIds r), r { freeIds = tail (freeIds r) })
    o <- liftIO $ newObject i
    r'' <- either (liftIO . throwIO) return $ regIns (coerce o) r'
    return (o, r'')
