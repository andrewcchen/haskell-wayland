{-# LANGUAGE FlexibleContexts #-}

module Graphics.Wayland.Internal.Marshal
    ( ArgType(..)
    , ArgSpec
    , InterfaceSpec
    , readArg, writeArg, sizeArg
    ) where

import Graphics.Wayland.Internal.Registry

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Bits
import Data.Coerce
import Data.Dynamic
import Data.Int
import Data.Maybe
import Data.Vector.Storable.ByteString
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
--import System.Posix.Types

data ArgType = ArgInt
             | ArgUInt
             | ArgFixed
             | ArgString
             | ArgObject TypeRep ArgNullable
             | ArgNewId TypeRep
             | ArgArray
             | ArgFd
type ArgName = String
type ArgNullable = Bool
type ArgSpec = (ArgName,ArgType)

type InterfaceName = String
type InterfaceSpec = (InterfaceName,[ArgSpec])

infixl 1 >>^
(>>^) :: Monad m => m a -> m b -> m a
(>>^) a b = a >>= (\x -> b >> return x)

from :: Typeable a => Dynamic -> a
from = fromJust . fromDynamic -- todo give better error message

readArg :: ArgType -> StateT (V.Vector Word32) (Either String) Dynamic
readArg argType = case argType of
    ArgUInt -> toDyn <$> readWord
    ArgInt -> toDyn <$> toInt32 <$> readWord
    ArgFixed -> toDyn <$> fixedToDouble <$> readWord
    ArgNewId _ -> toDyn <$> WlId <$> (checkNewId =<< readWord)
    ArgObject _ False -> toDyn <$> (checkObject =<< readWord)
    ArgObject _ True -> toDyn <$> mkMaybeObject <$> readWord
    ArgString -> toDyn <$> vectorToByteString <$> readArray
    ArgArray -> toDyn <$> readArray
    ArgFd -> error "internal error" -- file descriptors are sent in ancillary data
    where
    toInt32 x = fromIntegral x :: Int32
    checkNewId i = when (i == 0) (throwError "new_id cannot be 0") >> return i
    checkObject i = when (i == 0) (throwError "new_id cannot be 0") >> return i
    mkMaybeObject i = if (i == 0) then Nothing else Just i
    readArray = do
        size <- fromIntegral <$> readWord
        V.take size <$> toVector8 <$> V.force <$> readWords (div4RoundUp size)
    toVector8 v = V.unsafeCast v :: V.Vector Word8
    div4RoundUp s = (s+3) `quot` 4

writeArg :: ArgType -> Dynamic -> MV.MVector s Word32 -> ST s ()
writeArg argType arg v = case argType of
    ArgUInt -> MV.write v 0 $ from arg
    ArgInt -> MV.write v 0 $ fromIntegral (from arg :: Int32)
    ArgFixed -> MV.write v 0 $ doubleToFixed $ from arg
    ArgNewId _ -> MV.write v 0 $ coerce (from arg :: WlId)
    ArgObject _ False -> MV.write v 0 $ from arg
    ArgObject _ True -> MV.write v 0 $ fromMaybeObject $ from arg
    ArgString -> writeArray (byteStringToVector $ from arg :: V.Vector Word8)
    ArgArray -> writeArray (from arg :: V.Vector Word8)
    ArgFd -> error "internal error" -- file descriptors are sent in ancillary data
    where
    fromMaybeObject = maybe 0 id
    writeArray x = do
        MV.write v 0 $ fromIntegral $ V.length x
        copy (MV.unsafeCast $ MV.tail v) x
    copy t s = forM_ [0 .. V.length s - 1] $ \i ->
        -- TODO optimize this loop
        -- ideally we would want to copy by word instead of by byte
        MV.write t i $ s V.! i

sizeArg :: ArgType -> Dynamic -> Int
sizeArg argType arg = case argType of
    ArgUInt -> 4
    ArgInt -> 4
    ArgFixed -> 4
    ArgNewId _ -> 4
    ArgObject _ _ -> 4
    ArgString -> 4 + roundUpToMulOf4 (B.length $ from arg)
    ArgArray -> 4 + roundUpToMulOf4 (V.length (from arg :: V.Vector Word8))
    ArgFd -> 0 -- sent in ancillary data
    where roundUpToMulOf4 s = ((s+3) `quot` 4) * 4

readWord :: StateT (V.Vector Word32) (Either String) Word32
readWord = do
    whenM (gets V.null) $ throwError "message too short"
    gets V.head >>^ modify V.tail

readWords :: Int -> StateT (V.Vector Word32) (Either String) (V.Vector Word32)
readWords len = do
    whenM ((len >) <$> gets V.length) $ throwError "message too short"
    gets (V.take len) >>^ modify (V.drop len)

whenM :: Monad m => m Bool -> m () -> m ()
whenM m x = m >>= \p -> when p x

fixedToDouble :: Word32 -> Double
fixedToDouble w = fromIntegral sign * encodeFloat (fromIntegral $ w .&. 0x7FFFFFFF) (-8)
    where sign = w `shiftR` 31 * 2 - 1

doubleToFixed :: Double -> Word32
doubleToFixed d = sign `shiftL` 31 .|. round (scaleFloat 8 d) .&. 0x7FFFFFFF
    where sign = if d < 0 || isNegativeZero d then 1 else 0
