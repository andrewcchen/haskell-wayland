{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Wayland.Object where

import Data.Coerce
import Data.Function
import Data.Hashable
import Data.Typeable
import Data.Unique

--data Argument = ArgInt
--              | ArgUInt
--              | ArgFixed
--              | ArgString
--              | ArgObject TypeRep Bool
--              | ArgNewId TypeRep
--              | ArgArray
--              | ArgFd
--    deriving (Eq, Ord, Show)
--type Function = [Argument]
--
--data Interface = Interface TypeRep [Function]
--    deriving (Show)
--instance Eq Interface where
--    Interface a _ == Interface b _ = a == b
--    {-# INLINE (==) #-}
--instance Ord Interface where
--    Interface a _ `compare` Interface b _ = a `compare` b
--    {-# INLINE compare #-}

data Object = Object
    { objUnique :: Unique
    , objId :: Int
    , objType :: TypeRep
    }

instance Eq Object where
    (==) = (==) `on` objUnique
    {-# INLINE (==) #-}
instance Ord Object where
    compare = compare `on` objUnique
    {-# INLINE compare #-}
instance Hashable Object where
    hash = hash . objUnique
    {-# INLINE hash #-}
    hashWithSalt s x = s `hashWithSalt` objUnique x
    {-# INLINE hashWithSalt #-}

newObject :: forall a. (Typeable a, Coercible Object a) => Int -> IO a
newObject objId = do
    let objType = typeRep (Proxy :: Proxy a)
    objUnique <- newUnique
    return $ coerce Object{..}
{-# INLINE newObject #-}

fromObject :: forall a. (Typeable a, Coercible Object a) => Object -> a
fromObject obj = if otyp == rtyp then coerce obj
    else error $ "cannot cast object of type " ++ show otyp ++ " to " ++ show rtyp
    where otyp = objType obj
          rtyp = typeRep (Proxy :: Proxy a)
{-# INLINE fromObject #-}

toObject :: Coercible a Object => a -> Object
toObject = coerce
{-# INLINE toObject #-}
