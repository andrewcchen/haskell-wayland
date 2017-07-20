#include <wayland-util.h>

module Graphics.Wayland.Internal
    ( Fd(..)
    , NewId(..)
    , WlObject
    , WlNullableObject(..)
    , WlString(..)
    , WlArray(..)
    , WlFixed(..)
    , wlArgumentSize
    ) where

import Data.Bits
import Data.Coerce
import Data.Text
import Data.Text.Encoding
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types (Fd(..))
import qualified Data.ByteString as B


newtype NewId = NewId Word32

type WlObject = Ptr ()

newtype WlNullableObject = WlNullableObject (Maybe WlObject)
instance Storable WlNullableObject where
    sizeOf _ = sizeOf (nullPtr :: Ptr ())
    alignment _ = alignment (nullPtr :: Ptr ())
    peek p = do
        obj <- peek $ castPtr p
        if obj == nullPtr
           then return $ WlNullableObject Nothing
           else return $ coerce $ Just obj
    poke p (WlNullableObject Nothing) = poke (castPtr p) nullPtr
    poke p (WlNullableObject (Just obj)) = poke (castPtr p) obj

newtype WlString = WlString String
instance Storable WlString where
    sizeOf _ = sizeOf (nullPtr :: Ptr ())
    alignment _ = alignment (nullPtr :: Ptr ())
    peek p = do
        ptr <- peek $ castPtr p :: IO (Ptr CChar)
        coerce <$> unpack <$> decodeUtf8 <$> B.packCString ptr
--    poke p = undefined -- todo

newtype WlNullableString = WlNullableString (Maybe String)
instance Storable WlNullableString where
    sizeOf _ = sizeOf (nullPtr :: Ptr ())
    alignment _ = alignment (nullPtr :: Ptr ())
    peek p = do
        ptr <- peek $ castPtr p
        if ptr == nullPtr
           then return $ WlNullableString Nothing
           else coerce <$> Just <$> unpack <$> decodeUtf8 <$> B.packCString ptr
--    poke p = undefined -- todo

newtype WlArray = WlArray B.ByteString
instance Storable WlArray where
    sizeOf _ = sizeOf (nullPtr :: Ptr ())
    alignment _ = alignment (nullPtr :: Ptr ())
    peek p = do
        ptr <- peek $ castPtr p
        size <- #{peek struct wl_array, size} ptr
        dataPtr <- #{peek struct wl_array, data} ptr
        coerce <$> B.packCStringLen (dataPtr, size)
--    poke p = undefined -- todo

newtype WlNullableArray = WlArray B.ByteString
instance Storable WlArray where
    sizeOf _ = sizeOf (nullPtr :: Ptr ())
    alignment _ = alignment (nullPtr :: Ptr ())
    peek p = do
        ptr <- peek $ castPtr p
        size <- #{peek struct wl_array, size} ptr
        dataPtr <- #{peek struct wl_array, data} ptr
        coerce <$> B.packCStringLen (dataPtr, size)
--    poke p = undefined -- todo

newtype WlFixed = WlFixed Double
instance Storable WlFixed where
    sizeOf _ = sizeOf (nullPtr :: Ptr ())
    alignment _ = alignment (nullPtr :: Ptr ())
    peek p = coerce <$> toDouble <$> peek (castPtr p)
    poke p = poke (castPtr p) . fromDouble . coerce

wlArgumentSize = #{size union wl_argument} :: Int

toDouble :: Word32 -> Double
toDouble w = fromIntegral sign * encodeFloat (fromIntegral $ w .&. 0x7FFFFFFF) (-8)
    where sign = w `shiftR` 31 * 2 - 1

fromDouble :: Double -> Word32
fromDouble d = sign `shiftL` 31 .|. round (scaleFloat 8 d) .&. 0x7FFFFFFF
    where sign = if d < 0 || isNegativeZero d then 1 else 0
