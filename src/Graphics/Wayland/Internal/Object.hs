{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Wayland.Internal.Object
    ( ArgType(..)
    , ArgSpec(..), argName, argType
    , FuncSpec(..), funcName, funcArgs
    , InterfaceSpec(..), interfaceName, interfaceRecvFuncs, interfaceSendFuncs
    , WlId(..)
    , Message(..)
    , Object, objId, objType, objInterfaceSpecs
    , unsafeNewObject
    ) where

import Control.Lens
import Data.Function
import Data.Hashable
import Data.Typeable
import Data.Unique
import Data.Word
--import Unsafe.Coerce

data ArgType = ArgInt
             | ArgUInt
             | ArgFixed
             | ArgString
             | ArgObject TypeRep ArgNullable
             | ArgNewId TypeRep
             | ArgArray
             | ArgFd
    deriving (Eq, Ord, Show)
type ArgNullable = Bool

data ArgSpec = ArgSpec
    { _argName :: String
    , _argType :: ArgType
    } deriving (Eq, Ord, Show)
makeLenses ''ArgSpec

data FuncSpec = FuncSpec
    { _funcName :: String
    , _funcArgs :: [ArgSpec]
    } deriving (Eq, Ord, Show)
makeLenses ''FuncSpec

data InterfaceSpec = InterfaceSpec
    { _interfaceName :: String
    , _interfaceRecvFuncs :: [FuncSpec]
    , _interfaceSendFuncs :: [FuncSpec]
    } deriving (Eq, Ord, Show)
makeLenses ''InterfaceSpec

newtype WlId = WlId Word32
    deriving (Eq, Ord, Show, Hashable)

data Message = Message
    { msgOpcode :: Int
    , msgSender :: WlId
    , msgArgs :: [Dynamic]
    }

data Object = Object
    { objUnique :: Unique
    , objId :: WlId
    , objType :: TypeRep
    , objInterfaceSpecs :: InterfaceSpec
    }

instance Eq Object where
    (==) = (==) `on` objUnique
instance Ord Object where
    compare = compare `on` objUnique
instance Hashable Object where
    hashWithSalt s x = s `hashWithSalt` objUnique x

unsafeNewObject :: TypeRep -> InterfaceSpec -> WlId -> IO Object
unsafeNewObject objType objInterfaceSpecs objId = do
    objUnique <- newUnique
    return Object{..}
