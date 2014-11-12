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

data Version = Version{ vMaj ::Int
                      , vMin ::Int
                      , vTyp ::Int
                      , vBld ::Int
                      } deriving (Eq, Show)

{#pointer *fc2Version as VersionPtr -> Version #}

data ConfigRom = 
  ConfigRom { confRomVendor        :: Int
            , confRomChipldH       :: Int
            , confRomChipldL       :: Int
            , confRomUnitSpec      :: Int
            , confRomUnitSWVer     :: Int
            , confRomUnitSubSWVer  :: Int
            , confRomVendorUnique0 :: Int
            , confRomVendorUnique1 :: Int
            , confRomVendorUnique2 :: Int
            , convRomVendorUnique3 :: Int
            , confRomPszKeyword    :: String
            , confRomReserved      :: Int
            } deriving (Show)

{#pointer *fc2ConfigROM as ConfigRomPtr -> ConfigRom #}
  

{#enum fc2InterfaceType as FlyCapInterface {underscoreToCase} 
  deriving (Show, Eq) #}

{#enum fc2DriverType as FlyCapDriver {underscoreToCase}
  deriving (Show, Eq) #}

{#enum fc2BusSpeed as FlyCapBusSpeed {underscoreToCase}
  deriving (Show, Eq) #}

{#enum fc2PCIeBusSpeed as FlyCapPCIeBusSpeed {underscoreToCase}
  deriving (Show, Eq) #}

{#enum fc2BayerTileFormat as FlyCapBayerTileFormat {underscoreToCase}
  deriving (Show, Eq) #}

{#pointer *fc2CameraInfo as CamInfoPtr -> CamInfo #}

data CamInfo = CamInfo 
  { camSerialNum       :: Int
  , camInterface       :: FlyCapInterface
  , camDriver          :: FlyCapDriver
  , camIsColor         :: Bool
  , camModel           :: String
  , camVendor          :: String
  , camSensorInfo      :: String
  , camSensorRes       :: String
  , camDriverName      :: String
  , camFirmwareVer     :: String
  , camFirmwareTBuild  :: String
  , camMaxBusSpeed     :: FlyCapBusSpeed
  , camPCIeBusSpeed    :: FlyCapPCIeSpeed
  , camBayerTileFormat :: FlyCapBayerTileFormat
  , camBusNumber       :: Int
  , camNodeNumber      :: Int
  , camIidcVer         :: Int
  , camConfigRom       :: ConfigRom
  , camGigMajorVersion :: Int
  , camGigMinorVersion :: Int
  , camUserDefName     :: String
  , camXmlURL1         :: String
  , camXmlURL2         :: String
  , camMacAddress      :: MacAddress
  , camIPAddress       :: IPAddress 
  , camSubnetMask      :: IPAddress
  , camDefaultGateway  :: IPAddress
  , camCcpStatus       :: Int
  , camApplicationIP   :: Int
  , camApplicationPort :: Int
  , camReserved        :: Int
  } deriving (Eq, Show)
               
data MacAddress = MacAddress Char Char Char Char Char Char
data IPAddress  = IPAddress  Char Char Char Char

{#pointer *fc2MACAddress as MacAddressPtr -> MacAddress #}
{#pointer *fc2IPAddress  as IPAddressPtr  -> IPAddress  #}

data CImage = CImage { imgHeight :: Int
                     , imgWidth  :: Int
                     , imgStride      :: CUInt  -- What is stride?
                     , imgData       :: BS.ByteString
                     , imgDataSize :: Int
                     , imgFormat      :: CInt -- an fc2PixelFormat enum
                     , imgBayerFormat :: FlyCapBayerTileFormat
                     , imgIMPl   :: Ptr ()  
                     } deriving (Show)
              
instance Storable CImage where
  sizeOf _ = (#size fc2Image)
  alignment _ = alignment (undefined :: CDouble) 
  peek ptr = do
    r <- (#peek fc2Image, rows) ptr
    c <- (#peek fc2Image, cols) ptr
    s <- (#peek fc2Image, stride) ptr
    p <- (#peek fc2Image, pData) ptr
    dS <- (#peek fc2Image, dataSize) ptr
    f <- (#peek fc2Image, format) ptr
    bF <- (#peek fc2Image, bayerFormat) ptr
    iI <- (#peek fc2Image, imageImpl) ptr
    return CImage {rows = r, cols = c, stride = s, pData = p, dataSize = dS, format = f, bayerFormat = bF, imageIMPl = iI}
  poke ptr (CImage r c s p dS f bF iI) = do 
    (#poke fc2Image, rows) ptr r
    (#poke fc2Image, cols) ptr c
    (#poke fc2Image, stride) ptr s
    (#poke fc2Image, pData) ptr p
    (#poke fc2Image, dataSize) ptr dS
    (#poke fc2Image, format) ptr f
    (#poke fc2Image, bayerFormat) ptr bF
    (#poke fc2Image, imageImpl) ptr iI

highToC :: (Eq a, Ord a, Eq b, Ord b) => [(a,b)] -> b -> a -> b
highToC constMap valDefault query = 
  maybe valDefault id (lookup query constMap)

cToHigh :: (Eq a, Ord a, Eq b, Ord b) => [(a,b)] -> a -> b -> a
cToHigh constMap valDefault query =
  maybe valDefault id (lookup query (fl constMap))
  where fl m = map (\(f,s) -> (s,f)) m
        
data VideoMode = VM160x120 | VM320x240 | VM640x480_YUV411 | VM640x480_YUV422 | VM640x480_RGB | VM640x480_Y8 | VM640x480_Y16 | VM800x600_YUV422 | VM800x600_RGB | VM800x600_Y8 | VM800x600_Y16 | VM1024x768_YUV422 | VM1024x768_RGB | VM1024x768_Y8 | VM1024x768_Y16 | VM1280x960_YUV422 | VM1280x960_RGB | VM1280x960_Y8 | VM1280x960_Y16 | VM1600x1200_YUV422 | VM1600x1200_RGB | VM1600x1200_Y8 | VM1600x1200_Y16 | VMFormat7 | VMNum | VMForce32b deriving (Ord,Eq,Show)

vmMap :: [(VideoMode, CInt)]
vmMap = [ ( VM800x600_Y8, #const FC2_VIDEOMODE_800x600Y8)
        , ( VM160x120, #const FC2_VIDEOMODE_160x120YUV444) 	
        , ( VM320x240, #const FC2_VIDEOMODE_320x240YUV422)
        , ( VM640x480_YUV411, #const FC2_VIDEOMODE_640x480YUV411)
        , ( VM640x480_YUV422, #const FC2_VIDEOMODE_640x480YUV422)
        , ( VM640x480_RGB, #const FC2_VIDEOMODE_640x480RGB)
        , ( VM640x480_Y8, #const FC2_VIDEOMODE_640x480Y8)
        , ( VM640x480_Y16, #const FC2_VIDEOMODE_640x480Y16)
        , ( VM800x600_YUV422, #const FC2_VIDEOMODE_800x600YUV422)
        , ( VM800x600_RGB, #const FC2_VIDEOMODE_800x600RGB)
        , ( VM800x600_Y16, #const FC2_VIDEOMODE_800x600Y16)
        , ( VM1024x768_YUV422, #const FC2_VIDEOMODE_1024x768YUV422)
        , ( VM1024x768_RGB, #const FC2_VIDEOMODE_1024x768RGB) 
        , ( VM1024x768_Y8, #const FC2_VIDEOMODE_1024x768Y8)
        , ( VM1024x768_Y16, #const FC2_VIDEOMODE_1024x768Y16)
        , ( VM1280x960_YUV422, #const FC2_VIDEOMODE_1280x960YUV422)
        , ( VM1280x960_RGB, #const FC2_VIDEOMODE_1280x960RGB)
        , ( VM1280x960_Y8, #const FC2_VIDEOMODE_1280x960Y8)
        , ( VM1280x960_Y16, #const FC2_VIDEOMODE_1280x960Y16) 
        , (VM1600x1200_YUV422, #const FC2_VIDEOMODE_1600x1200YUV422)
        , (VM1600x1200_RGB, #const FC2_VIDEOMODE_1600x1200RGB)
        , (VM1600x1200_Y8, #const FC2_VIDEOMODE_1600x1200Y8)
        , (VM1600x1200_Y16, #const FC2_VIDEOMODE_1600x1200Y16)
        , (VMFormat7, #const FC2_VIDEOMODE_FORMAT7)
        , (VMNum, #const FC2_NUM_VIDEOMODES)
        , (VMForce32b, #const FC2_VIDEOMODE_FORCE_32BITS)]

vmFromC :: CInt -> VideoMode
vmFromC = cToHigh vmMap VM640x480_Y8

vmToC :: VideoMode -> CInt
vmToC = highToC vmMap (snd (head vmMap))

data FrameRate = Fr1_875 |Fr3_75 | Fr7_5 | Fr_15 | Fr_30 | Fr_60 | Fr_120 | Fr_240 | FrFormat7 | FrNumFR | FrForce32b 
               deriving (Eq, Ord, Show)
  
frMap :: [(FrameRate, CInt)]  
frMap = [ (Fr_30, #const FC2_FRAMERATE_30)
        , (Fr1_875, #const FC2_FRAMERATE_1_875)
        , (Fr3_75, #const FC2_FRAMERATE_3_75)
        , (Fr7_5, #const FC2_FRAMERATE_7_5)
        , (Fr_15, #const FC2_FRAMERATE_15)
        , (Fr_60, #const FC2_FRAMERATE_60)
        , (Fr_120, #const FC2_FRAMERATE_120)
        , (Fr_240, #const FC2_FRAMERATE_240)
        , (FrFormat7, #const FC2_FRAMERATE_FORMAT7)
        , (FrNumFR, #const FC2_NUM_FRAMERATES)
        , (FrForce32b, #const FC2_FRAMERATE_FORCE_32BITS) ]



frFromC :: CInt -> FrameRate
frFromC = cToHigh frMap Fr_30 
  
frToC :: FrameRate -> CInt
frToC =  highToC frMap (snd (head frMap))

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
 
