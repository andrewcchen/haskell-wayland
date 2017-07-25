{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, TemplateHaskell #-}

module Graphics.Wayland.Internal.Marshal
    ( decodeArgs
    , readArg, writeArg, sizeArg
    , fromDyn'
    ) where

import Graphics.Wayland.Internal.Object

import Control.Lens.Tuple
import Control.Lens.Zoom
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.State.Lazy
import Data.Bits
import Data.Coerce
import Data.Dynamic
import Data.Int
import Data.Maybe
import Data.Vector.Storable.ByteString
import Data.Word
import System.Posix.Types
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Lens
import Data.Hashable (Hashable(..))

decodeArgs :: [ArgSpec] -> V.Vector Word32 -> [Fd] -> Either String [Dynamic]
decodeArgs argSpecs buf fds = do
    flip evalStateT (buf,fds) $ forM argSpecs $ \arg -> case arg^.argType of
        ArgFd -> zoom _2 $ state $ \fds' -> (toDyn $ head fds', tail fds')
        typ -> zoom _1 $ readArg typ `catchError` (\e -> throwError $ e++" while decoding argument "++(arg^.argName))

readArg :: ArgType -> StateT (V.Vector Word32) (Either String) Dynamic
readArg = \case
    ArgUInt -> toDyn <$> readWord
    ArgInt -> toDyn <$> toInt32 <$> readWord
    ArgFixed -> toDyn <$> fixedToDouble <$> readWord
    ArgNewId _ -> toDyn <$> WlId <$> (checkNewId =<< readWord)
    ArgObject _ False -> toDyn <$> (checkObject =<< readWord)
    ArgObject _ True -> toDyn <$> mkMaybeObject <$> readWord
    ArgString -> toDyn <$> vectorToByteString <$> readArray
    ArgArray -> toDyn <$> readArray
    ArgFd -> error "wat" -- file descriptors are sent in ancillary data
    where
    toInt32 x = fromIntegral x :: Int32
    checkNewId i = when (i == 0) (throwError "new id cannot be 0") >> return i
    checkObject i = when (i == 0) (throwError "object id cannot be 0") >> return i
    mkMaybeObject i = if (i == 0) then Nothing else Just i
    readArray = do
        len <- fromIntegral <$> readWord
        V.take len <$> toVector8 <$> V.force <$> readWords (div4RoundUp len)
    toVector8 v = V.unsafeCast v :: V.Vector Word8
    div4RoundUp s = (s+3) `quot` 4

writeArg :: PrimMonad m => ArgType -> Dynamic -> StateT (MV.MVector (PrimState m) Word32) m ()
writeArg typ arg = case typ of
    ArgUInt -> writeWord $ fromDyn' arg
    ArgInt -> writeWord $ fromIntegral (fromDyn' arg :: Int32)
    ArgFixed -> writeWord $ doubleToFixed $ fromDyn' arg
    ArgNewId _ -> writeWord $ coerce (fromDyn' arg :: WlId)
    ArgObject _ False -> writeWord $ fromDyn' arg
    ArgObject _ True -> writeWord $ fromMaybeObject $ fromDyn' arg
    ArgString -> writeArray (byteStringToVector $ fromDyn' arg :: V.Vector Word8)
    ArgArray -> writeArray (fromDyn' arg :: V.Vector Word8)
    ArgFd -> error "wat" -- file descriptors are sent in ancillary data
    where
    fromMaybeObject = maybe 0 id
    writeArray a = do
        let len = V.length a
        writeWord $ fromIntegral $ len
        get >>= \v -> lift (copy (MV.unsafeCast v) a)
        modify $ MV.drop $ div4RoundUp len
    copy t s = forM_ [0 .. V.length s - 1] $ \i ->
        -- TODO optimize this loop
        -- ideally we would want to copy by word instead of by byte
        MV.write t i $ s V.! i
    div4RoundUp s = (s+3) `quot` 4

sizeArg :: ArgType -> Dynamic -> Int
sizeArg typ arg = case typ of
    ArgUInt -> 4
    ArgInt -> 4
    ArgFixed -> 4
    ArgNewId _ -> 4
    ArgObject _ _ -> 4
    ArgString -> 4 + roundUpToMulOf4 (B.length $ fromDyn' arg)
    ArgArray -> 4 + roundUpToMulOf4 (V.length (fromDyn' arg :: V.Vector Word8))
    ArgFd -> 0 -- sent in ancillary data
    where roundUpToMulOf4 s = ((s+3) `quot` 4) * 4

fromDyn' :: Typeable a => Dynamic -> a
fromDyn' = fromJust . fromDynamic -- todo give better error message

readWord :: StateT (V.Vector Word32) (Either String) Word32
readWord = do
    whenM (gets V.null) $ throwError "message too short"
    gets V.head >>^ modify V.tail

readWords :: Int -> StateT (V.Vector Word32) (Either String) (V.Vector Word32)
readWords len = do
    whenM ((len >) <$> gets V.length) $ throwError "message too short"
    gets (V.take len) >>^ modify (V.drop len)

writeWord :: PrimMonad m => Word32 -> StateT (MV.MVector (PrimState m) Word32) m ()
writeWord w = get >>= (\v -> lift (MV.write v 0 w)) >> modify MV.tail

infixl 1 >>^
(>>^) :: Monad m => m a -> m b -> m a
(>>^) a b = a >>= (\x -> b >> return x)

whenM :: Monad m => m Bool -> m () -> m ()
whenM m x = m >>= \p -> when p x

fixedToDouble :: Word32 -> Double
fixedToDouble w = fromIntegral sign * encodeFloat (fromIntegral $ w .&. 0x7FFFFFFF) (-8)
    where sign = w `shiftR` 31 * 2 - 1

doubleToFixed :: Double -> Word32
doubleToFixed d = sign `shiftL` 31 .|. round (scaleFloat 8 d) .&. 0x7FFFFFFF
    where sign = if d < 0 || isNegativeZero d then 1 else 0
