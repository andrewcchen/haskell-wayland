--{-# LANGUAGE KindSignatures, RankNTypes, RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Wayland.Registry where
-- TODO export list

--import Graphics.Wayland.Marshal
import Graphics.Wayland.Object

import Control.Exception
import Data.Coerce
import Control.Monad
import Data.Dynamic

import qualified Data.IntMap.Lazy as M

data Registry = Registry
    { nextId :: Int
    , freeIds :: [Int]
    , objs :: M.IntMap Object
    }

data RegistryError = ObjectDoesNotExist Int
                   | ObjectAlreadyExists Int
                   | ObjectTypeMismatch TypeRep TypeRep
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

regGet :: forall a. (Typeable a, Coercible Object a) => Int -> Registry -> IO a
regGet i r = do
    o <- maybe (throwIO (ObjectDoesNotExist i)) return $ M.lookup i (objs r)
    let expected = objType o
        actual = typeRep (Proxy :: Proxy a)
    when (expected /= actual) $ throwIO (ObjectTypeMismatch expected actual)
    return $ coerce o

regDel :: Object -> Registry -> IO Registry
regDel obj r = do
    let i = objId obj
    unless (M.member i (objs r)) $ throwIO (ObjectDoesNotExist i)
    return r { freeIds = i : freeIds r
             , objs = M.delete i (objs r)
             }

regIns :: Object -> Registry -> IO Registry
regIns obj r = do
    let i = objId obj
    when (M.member i (objs r)) $ throwIO (ObjectAlreadyExists i)
    return r { objs = M.insert i obj (objs r) }

regNewObj :: (Typeable a, Coercible Object a) => Registry -> IO (Registry, a)
regNewObj r = do
    let (i, r') = if null (freeIds r)
                     then (nextId r, r { nextId = nextId r + 1 })
                     else (head (freeIds r), r { freeIds = tail (freeIds r) })
    o <- newObject i
    r'' <- regIns (coerce o) r'
    return (r'', o)
