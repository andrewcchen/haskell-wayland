module Graphics.Wayland.Server.Example
    ( WlExampleInterface(..)
    ) where

import Control.Monad
import Data.Coerce
import Data.Int
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B

import Graphics.Wayland.Server.Internal
--import Graphics.Wayland.Server.Types
import Graphics.Wayland.Server.Resource


foreign import ccall "&wl_example_interface"
    wl_example_interface :: WlInterface

data WlExampleInterface = WlExampleInterface
    { exampleFuncA :: WlClient -> WlResource -> Int32 -> Word32 -> Double -> String -> WlResource -> B.ByteString -> Maybe WlResource -> IO ()
    , giveMeNewId :: WlClient -> WlResource -> NewId -> IO ()
    , giveMeFd :: WlClient -> WlResource -> Fd -> IO ()
    }

wlExampleDispatcher :: WlExampleInterface -> WlResource -> Word32 -> Ptr () -> IO ()
wlExampleDispatcher i r opcode args = do
    let p i = peekByteOff args $ i * wlArgumentSize
        f i = coerce <$> (p i :: IO WlFixed)
        s i = coerce <$> (p i :: IO WlString)
        o i = coerce <$> (p i :: IO WlObject)
        mo i = coerce <$> (p i :: IO WlNullableObject)
        id i = coerce <$> (p i :: IO Word32)
        a i = coerce <$> (p i :: IO WlArray)
        fd i = coerce <$> (p i :: IO CInt)
    c <- wlResourceGetClient r
    case opcode of
        --0 -> join $ exampleFuncA i c r <$> p 0 <*> p 1 <*> f 2 <*> s 3 <*> o 4 <*> a 5 <*> mo 6
        1 -> join $ giveMeNewId i c r <$> id 0
        2 -> join $ giveMeFd i c r <$> fd 0

instance WaylandInterface WlExampleInterface where
    wlInterface = const wl_example_interface
    wlDispatcher = wlExampleDispatcher
