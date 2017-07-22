{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Graphics.Wayland.Internal.Connection
    ( Message(..)
    -- , readMessage, sendMessage
    ) where

import Graphics.Wayland.Internal.Marshal
import Graphics.Wayland.Internal.Registry
import qualified Graphics.Wayland.Internal.Socket as Sock

import Control.Exception.Base
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Dynamic
import Data.Maybe
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

-- todo encode specs into message
data Message = Message
    { msgSender :: WlId
    , msgOpcode :: Int
    , msgArguments :: [Dynamic]
    }

-- todo handle EAGAIN

recvMessageHeader :: Sock.Socket -> IO (Maybe (WlId, Int, Int))
recvMessageHeader sock = allocaBytesAligned 8 4 $ \p -> do
    cnt <- liftIO $ Sock.read sock p 8
    if cnt < 8 then return Nothing else do
        sender <- WlId <$> (peek $ p `plusPtr` 0 :: IO Word32)
        opcode <- fromIntegral <$> (peek $ p `plusPtr` 4 :: IO Word16)
        len <- fromIntegral <$> (peek $ p `plusPtr` 6 :: IO Word16)
        return $ Just (sender, opcode, len-8)

recvMessageBody :: Sock.Socket -> InterfaceSpec -> Int -> IO (Maybe (V.Vector Word32))
recvMessageBody sock (_argSpecs) len = do
    buf <- MV.unsafeNew (len `quot'` 4) :: IO (MV.MVector RealWorld Word32)
    MV.unsafeWith buf $ \p -> do
        cnt <- Sock.read sock (castPtr p) len
        if cnt < len
           then return Nothing
           else Just <$> V.unsafeFreeze buf
    where quot' a b = (a+b-1) `quot` b

{-
-- todo use vector for buf
readMessage :: (WlId -> Int -> InterfaceSpec) -> Sock.Socket -> IO (Either String Message)
readMessage query sock = runExceptT $ do
    undefined
    buf' <- MV.new 2 :: IO (MV.MVector Word32) -- 32bit vector for alignment purposes
    --ptr <- liftIO $ mallocForeignPtrBytes 0x10000
    (msgSender, msgOpcode, len) <- ExceptT $
        MV.unsafeWith buf' (readMessageHeader sock . castPtr)
    let len' = len - 8
        argSpecs = snd $ query msgSender msgOpcode
        argTypes = map snd argSpecs
    vec <- readMessageBody sock ptr len'
    msgArguments <- ExceptT $ fmap (flip evalStateT vec . sequence)
        $ forM argTypes $ \x -> case x of
            ArgFd -> return <$> toDyn <$> readFd sock
            argType -> return $ readArg argType
    return Message{..}

-- TODO set fd to CLOEXEC
readFd :: Sock.Socket -> IO (Either String Fd)
readFd sock = do
    fd <- recvFd sock
    if fd < 0
       then return $ Left "cannot read file descriptor"
       else return $ Right $ Fd fd

readMessageHeader :: Sock.Socket -> Ptr Word8 -> IO (Either String (WlId, Int, Int))
readMessageHeader sock p = runExceptT $ do
    undefined
    lenRead <- liftIO $ recvBuf sock p 8
    when (lenRead /= 8) $ throwError ("read "++show lenRead++" bytes of header instead of 8 expected")
    sender <- liftIO $ WlId <$> (peek $ p `plusPtr` 0 :: IO Word32)
    opcode <- liftIO $ fromIntegral <$> (peek $ p `plusPtr` 4 :: IO Word16)
    len <- liftIO $ fromIntegral <$> (peek $ p `plusPtr` 6 :: IO Word16)
    when (len `rem` 4 /= 0) $ throwError ("message length "++show len++" is not a multiple of 4 bytes")
    return (sender, opcode, len)

readMessageBody :: Sock.Socket -> ForeignPtr Word8 -> Int -> ExceptT String IO (V.Vector Word32)
readMessageBody sock fp len = do
    alignOffset <- ExceptT $ withForeignPtr fp $ \p' -> runExceptT $ do
        let p = alignPtr p' 4
        lenRead <- liftIO $ recvBuf sock p len
        when (lenRead /= len) $ throwError ("read "++show lenRead++" bytes instead of "++show len++" expected")
        return $ p `minusPtr` p'
    return $ V.unsafeCast $ V.unsafeFromForeignPtr fp alignOffset len

sendMessage :: InterfaceSpec -> Sock.Socket -> Message -> IO ()
sendMessage (_,argSpecs) sock Message{..} = assert (len < 0x10000) $ do
    buf0 <- MV.new (0x10000 `quot` 4) :: IO (MV.MVector RealWorld Word32)
    let buf4 = MV.unsafeCast (MV.tail buf0) :: MV.MVector RealWorld Word16
        buf6 = MV.tail buf4
        buf8 = MV.drop 2 buf0
    MV.write buf0 0 $ coerce $ msgSender
    MV.write buf4 0 $ fromIntegral msgOpcode
    MV.write buf6 0 $ fromIntegral len
    -- todo assert args len
    flip evalStateT buf8 $ forM_ (zip argTypes msgArguments) $ \case
        (ArgFd, arg) -> liftIO $ sendFd sock $ coerce $ fromDyn' arg
        (argType, arg) -> stToIO' $ writeArg argType arg
    bytesSent <- MV.unsafeWith buf0 $ \p -> sendBuf sock (castPtr p) len
    when (bytesSent < len) $ error "failed to send message" -- todo use ExceptT?
    where argTypes = map snd argSpecs
          len = 8 + (foldr (+) 0 $ map (uncurry sizeArg) $ zip argTypes msgArguments)
          fromDyn' x = fromJust $ fromDynamic x :: Fd
          stToIO' = mapStateT stToIO
-}
