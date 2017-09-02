{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE LambdaCase #-}

module Graphics.Wayland.Marshal where

--import Graphics.Wayland.Object

--import Control.Lens.Tuple
--import Control.Lens.Zoom
--import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.State
import Data.Bits
--import Data.Dynamic
import Data.Vector.Storable.ByteString
import Data.Word
--import System.Posix.Types
--import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Exception
import Control.Monad.Catch
import Data.ByteString.UTF8
import Data.Int


data DecodeError = ObjectIdNull
                 | NewIdNull
                 | MsgTooShort
                 | UnknownOpcode
    deriving (Show)
--TODO
--instance Show DecodeError where
--    show e = undefined
instance Exception DecodeError



readInt :: (MonadState (V.Vector Word32) m, MonadThrow m) => m Int
readInt = fromIntegral <$> (fromIntegral :: Word32 -> Int32) <$> readWord
{-# INLINABLE readInt #-}

readUInt :: (MonadState (V.Vector Word32) m, MonadThrow m) => m Word
readUInt = fromIntegral <$> readWord
{-# INLINABLE readUInt #-}

readFixed :: (MonadState (V.Vector Word32) m, MonadThrow m) => m Double
readFixed = fixedToDouble <$> readWord
{-# INLINABLE readFixed #-}

readObj :: (MonadState (V.Vector Word32) m, MonadThrow m) => m Int
readObj = do
    i <- fromIntegral <$> readWord
    when (i == 0) $ throwM ObjectIdNull
    return i
{-# INLINABLE readObj #-}

readObjMaybe :: (MonadState (V.Vector Word32) m, MonadThrow m) => m (Maybe Int)
readObjMaybe = do
    i <- fromIntegral <$> readWord
    return $ if i == 0 then Nothing else Just i
{-# INLINABLE readObjMaybe #-}

readNewId :: (MonadState (V.Vector Word32) m, MonadThrow m) => m Int
readNewId = do
    i <- fromIntegral <$> readWord
    when (i == 0) $ throwM NewIdNull
    return i
{-# INLINABLE readNewId #-}

readArray :: (MonadState (V.Vector Word32) m, MonadThrow m) => m (V.Vector Word8)
readArray = do
    len <- fromIntegral <$> readWord
    bs <- readWords ((len+3) `quot` 4)
    return $ V.take len $ V.unsafeCast bs
{-# INLINABLE readArray #-}

readUtf8 :: (MonadState (V.Vector Word32) m, MonadThrow m) => m String
readUtf8 = toString <$> vectorToByteString <$> readArray
{-# INLINABLE readUtf8 #-}

writeInt :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
         => Int -> m ()
writeInt = writeWord . fromIntegral
{-# INLINABLE writeInt #-}

writeUInt :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
          => Word -> m ()
writeUInt = writeWord . fromIntegral
{-# INLINABLE writeUInt #-}

writeFixed :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
           => Double -> m ()
writeFixed = writeWord . doubleToFixed
{-# INLINABLE writeFixed #-}

writeObj :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
         => Int -> m ()
writeObj = writeInt
{-# INLINABLE writeObj #-}

writeObjMaybe :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
              => Maybe Int -> m ()
writeObjMaybe Nothing = writeInt 0
writeObjMaybe (Just x) = writeInt x
{-# INLINABLE writeObjMaybe #-}

writeNewId :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
           => Int -> m ()
writeNewId = writeInt
{-# INLINABLE writeNewId #-}

writeArray :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
           => V.Vector Word8 -> m ()
writeArray x = do
    writeInt $ V.length x
    v <- get
    forM_ [0 .. V.length x - 1] $ \i ->
        MV.write (MV.unsafeCast v) i $ x V.! i
    put $ MV.drop ((V.length x + 3) `quot` 4) v
{-# INLINABLE writeArray #-}

writeUtf8 :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
          => String -> m ()
writeUtf8 = writeArray . byteStringToVector .fromString
{-# INLINABLE writeUtf8 #-}

readWord :: (MonadState (V.Vector Word32) m, MonadThrow m) => m Word32
readWord = do
    empty <- gets V.null
    when empty $ throwM MsgTooShort
    w <- gets V.head
    modify V.tail
    return w

readWords :: (MonadState (V.Vector Word32) m, MonadThrow m)
          => Int -> m (V.Vector Word32)
readWords len = do
    l <- gets V.length
    when (l < len) $ throwM MsgTooShort
    ws <- gets $ V.take len
    modify $ V.drop len
    return ws

writeWord :: (PrimMonad m, MonadState (MV.MVector (PrimState m) Word32) m)
          => Word32 -> m ()
writeWord w = do
    v <- get
    MV.write v 0 w
    modify MV.tail

fixedToDouble :: Word32 -> Double
fixedToDouble w = sign * encodeFloat (fromIntegral (w .&. 0x7FFFFFFF)) (-8)
    where sign = fromIntegral $ w `shiftR` 31 * 2 - 1

doubleToFixed :: Double -> Word32
doubleToFixed d = sign `shiftL` 31 .|. round (scaleFloat 8 d) .&. 0x7FFFFFFF
    where sign = if d < 0 || isNegativeZero d then 1 else 0
