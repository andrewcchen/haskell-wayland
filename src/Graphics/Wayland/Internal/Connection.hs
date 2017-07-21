{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Graphics.Wayland.Internal.Connection
    (
    ) where

import Graphics.Wayland.Internal.Marshal
import Graphics.Wayland.Internal.Registry

--import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Data.Dynamic
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Network.Socket
import System.Posix.Types
import qualified Data.Vector.Storable as V

data ClientConn = ClientConn
    { clientSocket :: Socket
    }

data Message = Message
    { msgSender :: WlId
    , msgOpcode :: Int
    , msgArguments :: [Dynamic]
    }


readMessage :: InterfaceSpec -> ClientConn -> IO (Either String Message)
readMessage (_,argSpecs) conn = runExceptT $ do
    let sock = clientSocket conn
    ptr <- liftIO $ mallocForeignPtrBytes 0x10000
    (msgSender, msgOpcode, len) <- readMessageHeader sock ptr
    let len' = len - 8
    vec <- readMessageBody sock ptr len
    let argTypes = map snd argSpecs
    msgArguments <- ExceptT $ fmap (flip evalStateT vec . sequence)
        $ forM argTypes $ \x -> case x of
            ArgFd -> return <$> toDyn <$> readFd sock
            argType -> return $ readArg argType
    return Message{..}

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
