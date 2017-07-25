{-# LANGUAGE RecordWildCards #-}

module Graphics.Wayland.Server.Protocol.Example where

import Graphics.Wayland.Internal.Object
import Graphics.Wayland.Internal.Marshal

import Data.Typeable
import Unsafe.Coerce
--import Data.Coerce

newtype WlExample = WlExample Object
wlExampleInterface :: InterfaceSpec
wlExampleInterface = InterfaceSpec
    "wl_example"
    [FuncSpec "kek" [ArgSpec "a" (ArgObject (typeRep (Proxy :: Proxy WlExample)) False)
                    ,ArgSpec "b" ArgInt
                    ]
    ]
    [
    ]
    -- (\r m -> toDyn $ decodeWlExample r m)

data WlClient

data WlExampleMsg =
    WlExampleKek WlClient WlExample WlExample Int32

newWlExample :: WlId -> IO WlExample
newWlExample i = fmap unsafeCoerce $ unsafeNewObject (typeRep (Proxy :: Proxy WlExample)) wlExampleInterface i

decodeWlExample :: WlClient -> IORef Registry -> Message -> IO WlExampleMsg
decodeWlExample reg Message{..} = do
    case msgOpcode of
        0 -> undefined
