{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Wayland.Internal.Object
    ( ArgType(..)
    , ArgSpec(..), argName, argType
    , FuncSpec(..), funcName, funcArgs
    , InterfaceSpec(..), interfaceName, interfaceServerFuns, interfaceClientFuns
    , Object, objId, objType
    , newObject
    ) where

import Control.Lens
import Data.Function
import Data.Hashable
import Data.Typeable
import Data.Unique
import Data.Word

data ArgType = ArgInt
             | ArgUInt
             | ArgFixed
             | ArgString
             | ArgObject InterfaceSpec ArgNullable
             | ArgNewId InterfaceSpec
             | ArgArray
             | ArgFd
    deriving (Eq, Ord, Show)
type ArgNullable = Bool

data ArgSpec = ArgSpec
    { _argName :: String
    , _argType :: ArgType
    } deriving (Show)
makeLenses ''ArgSpec

data FuncSpec = FuncSpec
    { _funcName :: String
    , _funcArgs :: [ArgSpec]
    } deriving (Eq, Ord, Show)
makeLenses ''FuncSpec

data InterfaceSpec = InterfaceSpec
    { _interfaceName :: String
    , _interfaceServerFuns :: [FuncSpec]
    , _interfaceClientFuns :: [FuncSpec]
    } deriving (Show)
makeLenses ''InterfaceSpec

instance Eq InterfaceSpec where
    (==) = (==) `on` _interfaceName
instance Ord InterfaceSpec where
    compare = compare `on` _interfaceName

data Object = Object
    { objUnique :: Unique
    , objId :: Word32
    , objType :: InterfaceSpec
    }

instance Eq Object where
    (==) = (==) `on` objUnique
instance Ord Object where
    compare = compare `on` objUnique
instance Hashable Object where
    hashWithSalt s x = s `hashWithSalt` objUnique x

newObject :: InterfaceSpec -> Word32 -> IO Object
newObject objType objId = do
    objUnique <- newUnique
    return Object{..}
