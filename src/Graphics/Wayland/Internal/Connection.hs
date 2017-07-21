{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Graphics.Wayland.Internal.Connection
    (
    ) where

import Graphics.Wayland.Internal.Marshal
import Graphics.Wayland.Internal.Registry

import Control.Exception.Base
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Data.Coerce
import Data.Dynamic
import Data.Maybe
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Network.Socket
import System.Posix.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

data ClientConn = ClientConn
    { clientSocket :: Socket
    }

data Message = Message
    { msgSender :: WlId
    , msgOpcode :: Int
    , msgArguments :: [Dynamic]
    }

-- todo use vector for buf
readMessage :: (WlId -> Int -> InterfaceSpec) -> ClientConn -> IO (Either String Message)
readMessage query conn = runExceptT $ do
    let sock = clientSocket conn
    ptr <- liftIO $ mallocForeignPtrBytes 0x10000
    (msgSender, msgOpcode, len) <- readMessageHeader sock ptr
    let len' = len - 8
        argSpecs = snd $ query msgSender msgOpcode
        argTypes = map snd argSpecs
    vec <- readMessageBody sock ptr len
    msgArguments <- ExceptT $ fmap (flip evalStateT vec . sequence)
        $ forM argTypes $ \x -> case x of
            ArgFd -> return <$> toDyn <$> readFd sock
            argType -> return $ readArg argType
    return Message{..}

-- TODO set fd to CLOEXEC
readFd :: Socket -> IO (Either String Fd)
readFd sock = do
    fd <- recvFd sock
    if fd < 0
       then return $ Left "cannot read file descriptor"
       else return $ Right $ Fd fd

readMessageHeader :: Socket -> ForeignPtr Word8 -> ExceptT String IO (WlId, Int, Int)
readMessageHeader sock fp = ExceptT $ withForeignPtr fp $ \p' -> runExceptT $ do
    let p = alignPtr p' 4
    lenRead <- liftIO $ recvBuf sock p 8
    when (lenRead /= 8) $ throwError ("read "++show lenRead++" bytes of header instead of 8 expected")
    sender <- liftIO $ WlId <$> (peek $ p `plusPtr` 0 :: IO Word32)
    opcode <- liftIO $ fromIntegral <$> (peek $ p `plusPtr` 4 :: IO Word16)
    len <- liftIO $ fromIntegral <$> (peek $ p `plusPtr` 6 :: IO Word16)
    when (len `rem` 4 /= 0) $ throwError ("message length "++show len++" is not a multiple of 4 bytes")
    return (sender, opcode, len)

readMessageBody :: Socket -> ForeignPtr Word8 -> Int -> ExceptT String IO (V.Vector Word32)
readMessageBody sock fp len = do
    alignment <- ExceptT $ withForeignPtr fp $ \p' -> runExceptT $ do
        let p = alignPtr p' 4
        lenRead <- liftIO $ recvBuf sock p len
        when (lenRead /= len) $ throwError ("read "++show lenRead++" bytes instead of "++show len++" expected")
        return $ p `minusPtr` p'
    return $ V.unsafeCast $ V.unsafeFromForeignPtr fp alignment len

sendMessage :: Int -> InterfaceSpec -> ClientConn -> Message -> IO ()
sendMessage opcode (_,argSpecs) conn Message{..} = assert (len < 0x10000) $ do
    let sock = clientSocket conn
    buf0 <- MV.new (0x10000 `quot` 4) :: IO (MV.MVector RealWorld Word32)
    let buf4 = MV.unsafeCast (MV.tail buf0) :: MV.MVector RealWorld Word16
        buf6 = MV.tail buf4
        buf8 = MV.drop 2 buf0
    MV.write buf0 0 $ coerce $ msgSender
    MV.write buf4 0 $ fromIntegral opcode
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
