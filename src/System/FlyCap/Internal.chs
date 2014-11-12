{-# LANGUAGE ForeignFunctionInterface #-}

module System.FlyCap.Internal where

import Control.Monad.Trans.Reader
import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Codec.Picture
import GHC.Word
import qualified Data.ByteString as B


#include <FlyCapture2_C.h>
#include <FlyCapture2Defs_C.h>

newtype FlyCap a = FlyCap { unFlyCap :: ReaderT Context IO a }
  deriving (MonadReader Context, Monad, Applicative, Functor)

-- raw c imports here

type Context = Ptr () --fc2Context is a void pointer in C

{#enum fc2Error as FlyCapError {underscoreToCase} deriving (Show,Eq) #}

type ImageData = Ptr CUChar

newtype Guid = Guid Int

{#pointer *fc2PGRGuid as GuidPtr -> Guid #}                                                                                  

data Version = Version{ major'Version :: Int
                      , minor'Version :: Int
                      , type'Version  :: Int
                      , build'Version :: Int
                      } deriving (Eq, Show)

instance Storable Version where
  sizeof _    = {#sizeof Version #}
  alignment _ = 4
  peek p = Version
           <$> liftM fromIntegral ({#get fc2Version->major #} p)
           <*> liftM fromIntegral ({#get fc2Version->minor #} p)
           <*> liftM fromIntegral ({#get fc2Version->type  #} p)
           <*> liftM fromIntegral ({#get fc2Version->build #} p)
  poke p v = do
    

{#pointer *fc2Version as VersionPtr -> Version #}

data ConfigRom = 
  ConfigRom { vendor'ConfigRom        :: Int
            , chipldH'ConfigRom       :: Int
            , chipldL'ConfigRom       :: Int
            , unitSpec'ConfigRom      :: Int
            , unitSWVer'ConfigRom     :: Int
            , unitSubSWVer'ConfigRom  :: Int
            , vendorUnique0'ConfigRom :: Int
            , vendorUnique1'ConfigRom :: Int
            , vendorUnique2'ConfigRom :: Int
            , vendorUnique3'ConfigRom :: Int
            , pszKeyword'ConfigRom    :: String
            , reserved'ConfigRom      :: Int
            } deriving (Show)

{#pointer *fc2ConfigROM as ConfigRomPtr -> ConfigRom #}
  

{#enum fc2InterfaceType as Interface {underscoreToCase} 
  deriving (Show, Eq) #}

{#enum fc2DriverType as Driver {underscoreToCase}
  deriving (Show, Eq) #}

{#enum fc2BusSpeed as BusSpeed {underscoreToCase}
  deriving (Show, Eq) #}

{#enum fc2PCIeBusSpeed as PCIeBusSpeed {underscoreToCase}
  deriving (Show, Eq) #}

{#enum fc2BayerTileFormat as BayerTileFormat {underscoreToCase}
  deriving (Show, Eq) #}

{#pointer *fc2CameraInfo as CamInfoPtr -> CamInfo #}

data CamInfo = CamInfo 
  { serialNum'CamInfo       :: Int
  , interface'CamInfo       :: Interface
  , driver'CamInfo          :: Driver
  , isColor'CamInfo         :: Bool
  , model'CamInfo           :: String
  , vendor'CamInfo          :: String
  , sensorInfo'CamInfo      :: String
  , sensorRes'CamInfo       :: String
  , driverName'CamInfo      :: String
  , firmwareVer'CamInfo     :: String
  , firmwareTBuild'CamInfo  :: String
  , maxBusSpeed'CamInfo     :: BusSpeed
  , pcieBusSpeed'CamInfo    :: PCIeSpeed
  , bayerTileFormat'CamInfo :: BayerTileFormat
  , busNumber'CamInfo       :: Int
  , nodeNumber'CamInfo      :: Int
  , iidcVer'CamInfo         :: Int
  , configRom'CamInfo       :: ConfigRom
  , gigMajorVersion'CamInfo :: Int
  , gigMinorVersion'CamInfo :: Int
  , userDefName'CamInfo     :: String
  , xmlURL1'CamInfo         :: String
  , xmlURL2'CamInfo         :: String
  , macAddress'CamInfo      :: MacAddress
  , ipAddress'CamInfo       :: IPAddress 
  , subnetMask'CamInfo      :: IPAddress
  , defaultGateway'CamInfo  :: IPAddress
  , ccpStatus'CamInfo       :: Int
  , applicationIP'CamInfo   :: Int
  , applicationPort'CamInfo :: Int
  , reserved'CamInfo        :: Int
  } deriving (Eq, Show)
               
data MacAddress = MacAddress Word8 Word8 Word8 Word8 Word8 Word8
data IPAddress  = IPAddress  Word8 Word8 Word8 Word8

{#pointer *fc2MACAddress as MacAddressPtr -> MacAddress #}
{#pointer *fc2IPAddress  as IPAddressPtr  -> IPAddress  #}

data CImage = CImage { height'CImage           :: Int
                     , hidth'CImage            :: Int
                     , stride'CImage           :: Int
                     , data'CImage             :: BS.ByteString
                     , dataSize'CImage         :: Int
                     , receivedDataSize'CImage :: Int
                     , format'CImage           :: Int
                     , bayerFormat'CImage      :: FlyCapBayerTileFormat
                     , impl'CImage             :: Ptr ()
                     } deriving (Show)

{#enum fc2VideoMode as VideoMode {underscoreToCase} deriving (Show, Eq) #}

{#enum fc2FrameRate as FrameRate {underscoreToCase} deriving (Show, Eq) #}

--functions used in tracker.c:  

foreign import ccall unsafe "FlyCapture2_C.h fc2GetNumOfCameras"
   fc2getNumOfCameras :: Context -> Ptr CUInt -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2CreateContext"
   fc2CreateContext :: Ptr (Context) -> IO Error
                      
foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraFromIndex"
   fc2GetCameraFromIndex :: Context -> CUInt -> Ptr PGRGuid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraFromSerialNumber"
   fc2GetCameraFromSerialNumber :: Context -> CUInt -> Ptr PGRGuid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2Connect"
  fc2Connect :: Context -> Ptr PGRGuid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetLibraryVersion"
  fc2GetLibraryVersion :: Ptr fc2Version -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraInfo"
  fc2GetCameraInfo :: Context -> Ptr CamInfo -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2SetVideoModeAndFrameRate"
  fc2SetVideoModeAndFrameRate :: Context -> CInt -> CInt -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2StartSyncCapture"
  fc2StartSyncCapture :: CUInt -> Ptr Context -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2StartCapture"
  fc2StartCapture :: Context -> IO Error
 
foreign import ccall unsafe "FlyCapture2_C.h fc2RetrieveBuffer"
  fc2RetrieveBuffer :: Context ->Ptr CImage -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2CreateImage" 
 fc2CreateImage :: Ptr CImage -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2StopCapture"
  fc2StopCapture :: Context -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetImageData"
  fc2GetImageData :: Ptr CImage -> Ptr ImageData -> IO Error
 
foreign import ccall unsafe "FlyCapture2_C.h fc2Disconnect"
  fc2Disconnect :: Context -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2DestroyContext"
  fc2DestroyContext :: Context -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2DestroyImage"
  fc2DestroyImage :: Ptr CImage -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2AVIAppend"
  fc2AVIAppend :: AVIContext -> Ptr CImage -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2AVIClose"
  fc2AVIClose :: AVIContext -> IO Error
                 
foreign import ccall unsafe "FlyCapture2_C.h fc2AVIOpen"
  fc2AVIOpen :: AVIContext -> Ptr CChar -> Ptr AVIOption -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2CreateAVI"
  fc2CreateAVI :: Ptr AVIContext -> IO Error
                  
foreign import ccall unsafe "FlyCapture2_C.h fc2DestroyAVI"
  fc2DestroyAVI :: AVIContext -> IO Error

ctoJImage :: CImage -> IO B.ByteString -- IO DynamicImage
ctoJImage (CImage r c _ p _ _ _ _ ) = do
  let h = fromIntegral r
  let w = fromIntegral c
  print $ "h was: " ++ show h
  print $ "w was: " ++ show w
  a <- peekArray (h*w) p -- arr: [CUChar]
  print $ "array was" ++ show a
  let arr = map (fromIntegral) a 
  let bs = B.pack arr
  return bs
 
