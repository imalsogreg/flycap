{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.FlyCap.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Codec.Picture
import GHC.Word
import System.IO.Unsafe
import qualified Data.ByteString as BS


#include <FlyCapture2_C.h>
#include <FlyCapture2Defs_C.h>

newtype FlyCap a = FlyCap { unFlyCap :: ReaderT Context IO a }
  deriving (MonadIO, MonadReader Context, Monad, Applicative, Functor)

runFlyCap :: FlyCap a -> IO a
runFlyCap a = do
  ctx <- fc1 <$> fc2CreateContext
  (runReaderT $ unFlyCap a) ctx

{#pointer *fc2Context as ContextPtr -> Context #}

newtype Context = Context { unContext :: Ptr () }  
  deriving (Eq, Show, Storable)

{#enum fc2Error as Error {underscoreToCase} deriving (Show,Eq) #}

{#fun fc2CreateContext as ^
 { alloca- `Context' peek* } -> `Error' #}

getNumOfCameras :: FlyCap Int
getNumOfCameras = do
  ctx <- ask
  liftIO $ (fromIntegral . fc1) <$> fc2GetNumOfCameras ctx

{#fun fc2GetNumOfCameras as ^
 { unContext `Context' , alloca- `CUInt' peek* } -> `Error' #}

getCameraFromIndex :: Int -> FlyCap Guid
getCameraFromIndex i = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetCameraFromIndex ctx i


type ImageData = Ptr CUChar

newtype Guid = Guid {unGuid :: [Int]} deriving (Eq, Show) 

{#pointer *fc2PGRGuid as GuidPtr -> Guid #}                                                           

{#fun fc2GetCameraFromIndex as ^
 { unContext `Context', `Int', alloca- `Guid' peek* } -> `Error' #}

------------------------------------------------------------------------------
instance Storable Guid where
  sizeOf _ = {#sizeof fc2PGRGuid #}
  alignment _ = 4
  peek p = do
    xs <- map fromIntegral <$> peekArray 4 
      (p `plusPtr` {#offsetof fc2PGRGuid->value #} :: Ptr CUInt)
    return $ Guid xs
  poke p (Guid xs) = do
    pokeArray (p `plusPtr` {#offsetof fc2PGRGuid->value #}) 
      (map fromIntegral xs :: [CUInt]) 


data MacAddress = MacAddress [Word8]
  deriving (Eq, Show)
data IPAddress  = IPAddress  [Word8]
  deriving (Eq, Show)

{#pointer *fc2MACAddress as MacAddressPtr -> MacAddress #}
{#pointer *fc2IPAddress  as IPAddressPtr  -> IPAddress  #}

instance Storable MacAddress where
  sizeOf _ = {# sizeof fc2MACAddress #}
  alignment _ = 4
  peek p = (MacAddress . map fromIntegral) <$> peekArray 6 
       (p `plusPtr` {#offsetof fc2MACAddress.octets #} :: Ptr CUChar)
  poke p (MacAddress xs) = pokeArray 
    (p `plusPtr` {#offsetof fc2MACAddress.octets #})
    (map fromIntegral xs :: [CUChar])

instance Storable IPAddress where
  sizeOf _ = {# sizeof fc2IPAddress #}
  alignment _ = 4
  peek p = (IPAddress . map fromIntegral) <$> peekArray 4 
       (p `plusPtr` {#offsetof fc2IPAddress.octets #} :: Ptr CUChar)
  poke p (IPAddress xs) = pokeArray 
    (p `plusPtr` {#offsetof fc2IPAddress.octets #})
    (map fromIntegral xs :: [CUChar])

data Version = Version{ major'Version :: Int
                      , minor'Version :: Int
                      , type'Version  :: Int
                      , build'Version :: Int
                      } deriving (Eq, Show)

{#pointer *fc2Version as VersionPtr -> Version #}

{#fun fc2GetLibraryVersion as ^
  { alloca- `Version' peek* } -> `Error' #}

version :: Version
version = fc1 $ unsafePerformIO fc2GetLibraryVersion

instance Storable Version where
  sizeOf _    = {#sizeof fc2Version #}
  alignment _ = 4
  peek p = Version
           <$> liftM fromIntegral ({#get fc2Version->major #} p)
           <*> liftM fromIntegral ({#get fc2Version->minor #} p)
           <*> liftM fromIntegral ({#get fc2Version->type  #} p)
           <*> liftM fromIntegral ({#get fc2Version->build #} p)
  poke p v = do
    {#set fc2Version->major #} p (fromIntegral $ major'Version v)
    {#set fc2Version->minor #} p (fromIntegral $ minor'Version v)
    {#set fc2Version->type  #} p (fromIntegral $ type'Version  v)
    {#set fc2Version->build #} p (fromIntegral $ build'Version v)


data ConfigRom = 
  ConfigRom { vendor'ConfigRom        :: Int
            , chipIdH'ConfigRom       :: Int
            , chipIdL'ConfigRom       :: Int
            , unitSpec'ConfigRom      :: Int
            , unitSWVer'ConfigRom     :: Int
            , unitSubSWVer'ConfigRom  :: Int
            , vendorUnique0'ConfigRom :: Int
            , vendorUnique1'ConfigRom :: Int
            , vendorUnique2'ConfigRom :: Int
            , vendorUnique3'ConfigRom :: Int
            , pszKeyword'ConfigRom    :: String
            , reserved'ConfigRom      :: [Int]
            } deriving (Eq, Show)

{#pointer *fc2ConfigROM as ConfigRomPtr -> ConfigRom #}

------------------------------------------------------------------------------
-- Util
pokeCString :: Ptr CChar -> String -> IO ()
pokeCString p str = pokeArray0 (castCharToCChar '\0') p 
                     (map castCharToCChar str)

instance Storable ConfigRom where
  sizeOf _ = {#sizeof fc2ConfigROM #}
  alignment _ = 4
  peek p = ConfigRom
    <$> liftM fromIntegral ({#get fc2ConfigROM->nodeVendorId #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->chipIdHi #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->chipIdLo  #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->unitSpecId #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->unitSWVer #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->unitSubSWVer #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->vendorUniqueInfo_0 #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->vendorUniqueInfo_1 #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->vendorUniqueInfo_2 #} p)
    <*> liftM fromIntegral ({#get fc2ConfigROM->vendorUniqueInfo_3 #} p)
    <*> peekCString (p `plusPtr` {#offsetof fc2ConfigROM->pszKeyword #})
    <*> map fromIntegral `liftM` peekArray 16 
        (p `plusPtr` {#offsetof fc2ConfigROM->reserved #} :: Ptr CUInt)
  poke p c = do
    {#set fc2ConfigROM->nodeVendorId       #} p 
      (fromIntegral $ vendor'ConfigRom  c)
    {#set fc2ConfigROM->chipIdHi           #} p 
      (fromIntegral $ chipIdH'ConfigRom c)
    {#set fc2ConfigROM->chipIdLo           #} p 
      (fromIntegral $ chipIdL'ConfigRom c)
    {#set fc2ConfigROM->unitSpecId         #} p 
      (fromIntegral $ unitSpec'ConfigRom c)
    {#set fc2ConfigROM->unitSubSWVer       #} p 
      (fromIntegral $ unitSWVer'ConfigRom c)
    {#set fc2ConfigROM->vendorUniqueInfo_0 #} p 
      (fromIntegral $ vendorUnique0'ConfigRom c)
    {#set fc2ConfigROM->vendorUniqueInfo_1 #} p 
      (fromIntegral $ vendorUnique1'ConfigRom c)
    {#set fc2ConfigROM->vendorUniqueInfo_2 #} p 
      (fromIntegral $ vendorUnique2'ConfigRom c)
    {#set fc2ConfigROM->vendorUniqueInfo_3 #} p 
      (fromIntegral $ vendorUnique3'ConfigRom c)
    pokeCString (p `plusPtr` {#offsetof fc2ConfigROM->pszKeyword #}) 
      (pszKeyword'ConfigRom c) 
    pokeArray (p `plusPtr` {#offsetof fc2ConfigROM->reserved #}) 
      (map fromIntegral $ reserved'ConfigRom c :: [CUInt])


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
  , pcieBusSpeed'CamInfo    :: PCIeBusSpeed
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
  , reserved'CamInfo        :: [Int]
  } deriving (Eq, Show)

{#fun fc2GetCameraInfo as ^
 { unContext `Context', alloca- `CamInfo' peek* } -> `Error' #}

getCamInfo :: FlyCap CamInfo
getCamInfo = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetCameraInfo ctx

instance Storable CamInfo where
  sizeOf _ = {#sizeof fc2CameraInfo #}
  alignment _ = 4
  peek p = CamInfo 
    <$> liftM fromIntegral ({#get fc2CameraInfo->serialNumber #} p)
    <*> liftM (toEnum . fromIntegral) ({#get fc2CameraInfo->interfaceType #} p)
    <*> liftM (toEnum . fromIntegral) ({#get fc2CameraInfo->driverType #} p)
    <*> liftM (toEnum . fromIntegral) ({#get fc2CameraInfo->isColorCamera #} p)
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->modelName #})
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->vendorName #})
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->sensorInfo #})
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->sensorResolution #})
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->driverName #})
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->firmwareVersion #})
    <*> peekCString  (p `plusPtr` {#offsetof fc2CameraInfo->firmwareBuildTime #})
    <*> liftM (toEnum . fromIntegral) ({#get fc2CameraInfo->maximumBusSpeed #} p)
    <*> liftM (toEnum . fromIntegral) ({#get fc2CameraInfo->pcieBusSpeed #} p)
    <*> liftM (toEnum . fromIntegral) ({#get fc2CameraInfo-> bayerTileFormat #} p)
    <*> liftM fromIntegral ({#get fc2CameraInfo->busNumber #} p)
    <*> liftM fromIntegral ({#get fc2CameraInfo->nodeNumber #} p)
    <*> liftM fromIntegral ({#get fc2CameraInfo->iidcVer #} p)
    <*> peek (p `plusPtr` {#offsetof fc2CameraInfo->configROM #})
    <*> liftM fromIntegral ({#get fc2CameraInfo->gigEMajorVersion #} p)
    <*> liftM fromIntegral ({#get fc2CameraInfo->gigEMinorVersion #} p)
    <*> peekCString (p `plusPtr` {#offsetof fc2CameraInfo->userDefinedName #})
    <*> peekCString (p `plusPtr` {#offsetof fc2CameraInfo->xmlURL1 #})
    <*> peekCString (p `plusPtr` {#offsetof fc2CameraInfo->xmlURL2 #})
    <*> peek (p `plusPtr` {#offsetof fc2CameraInfo->macAddress #})
    <*> peek (p `plusPtr` {#offsetof fc2CameraInfo->ipAddress #})
    <*> peek (p `plusPtr` {#offsetof fc2CameraInfo->subnetMask #})
    <*> peek (p `plusPtr` {#offsetof fc2CameraInfo->defaultGateway #})
    <*> liftM fromIntegral ({#get fc2CameraInfo->ccpStatus #} p)
    <*> liftM fromIntegral ({#get fc2CameraInfo->applicationIPAddress #} p)
    <*> liftM fromIntegral ({#get fc2CameraInfo->applicationPort #} p)
    <*> (map fromIntegral) `liftM` (peekArray 16 (p `plusPtr` {#offsetof fc2CameraInfo->reserved #}) :: IO [CUInt])

{#fun fc2Connect as ^
  { unContext `Context', poke `Guid' } -> `Error' #}

data CImage = CImage { height'CImage           :: Int
                     , hidth'CImage            :: Int
                     , stride'CImage           :: Int
                     , data'CImage             :: BS.ByteString
                     , dataSize'CImage         :: Int
                     , receivedDataSize'CImage :: Int
                     , format'CImage           :: Int
                     , bayerFormat'CImage      :: BayerTileFormat
                     , impl'CImage             :: Ptr ()
                     } deriving (Show)

{#enum fc2VideoMode as VideoMode {underscoreToCase} deriving (Show, Eq) #}

{#enum fc2FrameRate as FrameRate {underscoreToCase} deriving (Show, Eq) #}

--functions used in tracker.c:  

{-
foreign import ccall unsafe "FlyCapture2_C.h fc2GetNumOfCameras"
   fc2getNumOfCameras :: Context -> Ptr CUInt -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2CreateContext"
   fc2CreateContext :: Ptr (Context) -> IO Error
                      
foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraFromIndex"
   fc2GetCameraFromIndex :: Context -> CUInt -> Ptr Guid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraFromSerialNumber"
   fc2GetCameraFromSerialNumber :: Context -> CUInt -> Ptr Guid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2Connect"
  fc2Connect :: Context -> Ptr Guid -> IO Error

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
 
-}

fc1 :: (Error,a) -> a
fc1 (Fc2ErrorOk,a) = a
fc1 (e,_)          = error $ "FlyCapture2 error: " ++ show e

fc2 :: (Error,a,b)   -> (a,b) 
fc2 (Fc2ErrorOk,a,b) = (a,b)
fc2 (e,_,_)          = error $ "FlyCapture2 error: " ++ show e

fc3 :: (Error,a,b,c) -> (a,b,c)
fc3 (Fc2ErrorOk,a,b,c) = (a,b,c)
fc3 (e,_,_,_) = error $ "FlyCapture2 error: " ++ show e
