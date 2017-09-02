{-# LANGUAGE FlexibleContexts #-}
--, GeneralizedNewtypeDeriving, LambdaCase, TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Wayland.Marshal
    where

import Graphics.Wayland.Object

import Control.Lens.Tuple
import Control.Lens.Zoom
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.State.Lazy
import Data.Bits
--import Data.Coerce
import Data.Dynamic
--import Data.Int
--import Data.Maybe
import Data.Vector.Storable.ByteString
import Data.Word
import System.Posix.Types
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
--import Control.Lens
--import Data.Hashable (Hashable(..))
--import Data.List
import Control.Exception
import Control.Monad.Catch


data DecodeError = ObjectIdNull
                 | NewIdNull
                 | MsgTooShort
    deriving (Show)
--TODO
--instance Show DecodeError where
--    show e = undefined
instance Exception DecodeError

readArg :: MonadThrow m => Argument -> StateT (V.Vector Word32, [Fd]) m Dynamic
readArg typ = whenM (gets (V.null . fst)) (throwM MsgTooShort) >> case typ of
    ArgUInt -> toDyn <$> toInt <$> zoom _1 readWord
    ArgInt -> toDyn <$> toInt <$> zoom _1 readWord
    ArgFixed -> toDyn <$> fixedToDouble <$> zoom _1 readWord
    ArgObject _ False -> toDyn <$> (checkObject =<< zoom _1 readWord)
    ArgObject _ True -> toDyn <$> mkMaybeObject <$> zoom _1 readWord
    ArgNewId _ -> toDyn <$> (checkNewId =<< zoom _1 readWord)
    ArgString -> toDyn <$> vectorToByteString <$> zoom _1 readArray
    ArgArray -> toDyn <$> zoom _1 readArray
    ArgFd -> zoom _2 $ do
        -- assume we have the right amount of Fd's
        fd <- gets head
        modify tail
        return $ toDyn fd
    where
    toInt x = fromIntegral x :: Int
    toVector8 v = V.unsafeCast v :: V.Vector Word8
    checkNewId i = when (i == 0) (throwM ObjectIdNull) >> return i
    checkObject i = when (i == 0) (throwM NewIdNull) >> return i
    mkMaybeObject i = if (i == 0) then Nothing else Just i
    readArray = do
        len <- fromIntegral <$> readWord
        V.take len <$> toVector8 <$> V.force <$> readWords (div4RoundUp len)
    div4RoundUp s = (s+3) `quot` 4

writeArg :: PrimMonad m
         => Argument -> Dynamic -> StateT (MV.MVector (PrimState m) Word32, [Fd]) m ()
writeArg typ arg = case typ of
    ArgInt -> zoom _1 $ writeWord $ fromIntegral $ fromInt arg
    ArgUInt -> zoom _1 $ writeWord $ fromIntegral $ fromInt arg
    ArgFixed -> zoom _1 $ writeWord $ doubleToFixed $ fromDyn' arg
    ArgObject _ False -> zoom _1 $ writeWord $ fromDyn' arg
    ArgObject _ True -> zoom _1 $ writeWord $ fromMaybeObject $ fromDyn' arg
    ArgNewId _ -> zoom _1 $ writeWord $ fromDyn' arg
    ArgString -> zoom _1 $ writeArray $ byteStringToVector $ fromDyn' arg
    ArgArray -> zoom _1 $ writeArray $ fromDyn' arg
    ArgFd -> zoom _2 $ state $ \fds -> ((), fds ++ [fromDyn' arg :: Fd])
    where
    fromInt x = fromDyn' x :: Int
    fromMaybeObject = maybe 0 id
    writeArray :: PrimMonad m
               => V.Vector Word8 -> StateT (MV.MVector (PrimState m) Word32) m ()
    writeArray a = do
        let len = V.length a
        writeWord $ fromIntegral $ len
        get >>= \v -> copy (MV.unsafeCast v) a
        modify $ MV.drop $ div4RoundUp len
    copy t s = forM_ [0 .. V.length s - 1] $ \i ->
        -- TODO ideally we would want to copy by word instead of by byte
        MV.write t i $ s V.! i
    div4RoundUp s = (s+3) `quot` 4

argSize :: Argument -> Dynamic -> Int
argSize typ arg = case typ of
    ArgInt -> 4
    ArgUInt -> 4
    ArgFixed -> 4
    ArgObject _ _ -> 4
    ArgNewId _ -> 4
    ArgString -> 4 + roundUpToMulOf4 (B.length $ fromDyn' arg)
    ArgArray -> 4 + roundUpToMulOf4 (V.length (fromDyn' arg :: V.Vector Word8))
    ArgFd -> 0 -- sent in ancillary data
    where roundUpToMulOf4 s = ((s+3) `quot` 4) * 4

argIsFd :: Argument -> Bool
argIsFd ArgFd = True
argIsFd _ = False

readWord :: MonadThrow m => StateT (V.Vector Word32) m Word32
readWord = do
    whenM (gets V.null) $ throwM MsgTooShort
    w <- gets V.head
    modify V.tail
    return w
    --gets V.head >>^ modify V.tail

readWords :: MonadThrow m => Int -> StateT (V.Vector Word32) m (V.Vector Word32)
readWords len = do
    whenM ((len >) <$> gets V.length) $ throwM MsgTooShort
    ws <- gets $ V.take len
    modify $ V.drop len
    return ws
    --gets (V.take len) >>^ modify (V.drop len)

writeWord :: PrimMonad m => Word32 -> StateT (MV.MVector (PrimState m) Word32) m ()
writeWord w = do
    v <- get
    MV.write v 0 w
    modify MV.tail
    --get >>= (\v -> MV.write v 0 w) >> modify MV.tail

--infixl 1 >>^
--(>>^) :: Monad m => m a -> m b -> m a
--(>>^) a b = a >>= (\x -> b >> return x)

whenM :: Monad m => m Bool -> m () -> m ()
whenM m x = m >>= \p -> when p x

fixedToDouble :: Word32 -> Double
fixedToDouble w = fromIntegral sign * encodeFloat (fromIntegral $ w .&. 0x7FFFFFFF) (-8)
    where sign = w `shiftR` 31 * 2 - 1

doubleToFixed :: Double -> Word32
doubleToFixed d = sign `shiftL` 31 .|. round (scaleFloat 8 d) .&. 0x7FFFFFFF
    where sign = if d < 0 || isNegativeZero d then 1 else 0

fromDyn' :: Typeable a => Dynamic -> a
fromDyn' = maybe (error "fromDyn': type mismatch") id . fromDynamic






{-
decodeArgs :: [ArgSpec] -> V.Vector Word32 -> [Fd] -> Either String [Dynamic]
decodeArgs argSpecs buf fds = do
    flip evalStateT (buf,fds) $ forM argSpecs $ \arg -> case arg^.argType of
        ArgFd -> zoom _2 $ state $ \fds' -> (toDyn $ head fds', tail fds')
        typ -> zoom _1 $ readArg typ `catchError` (\e -> throwError $ e++" while decoding argument "++(arg^.argName))

readArg :: ArgType -> StateT (V.Vector Word32) (Either String) Dynamic
readArg = \case
    ArgInt -> toDyn <$> toInt32 <$> readWord
    ArgUInt -> toDyn <$> readWord
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

writeWord :: PrimMonad m => Word32 -> StateT (MV.MVector (PrimState m) Word32) m ()
writeWord w = get >>= (\v -> lift (MV.write v 0 w)) >> modify MV.tail
-}
