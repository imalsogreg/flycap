{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module System.FlyCap.FlyCapBase where

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Codec.Picture
import qualified Data.ByteString as B
import Control.Monad

#include <FlyCapture2_C.h>

-- raw c imports here

type Context = Ptr () --fc2Context is a void pointer in C
type Error = CInt
type ImageData = Ptr CUChar
type AVIContext = Ptr ()
data  AVIOption = AVIOption {frameRate :: CFloat, reservedList :: Ptr CUInt} 
instance Storable AVIOption where
  sizeOf _ = (#size fc2AVIOption)
  alignment _ = alignment (undefined :: CFloat)
  peek ptr = do
    fr <- (#peek fc2AVIOption, frameRate) ptr
    res <- (#peek fc2AVIOption, reserved) ptr
    return AVIOption {frameRate = fr, reservedList = res}
  poke ptr (AVIOption fr res) = do
    (#poke fc2AVIOption, frameRate) ptr fr
    (#poke fc2AVIOption, reserved) ptr res
    
data PGRGuid = PGRGuid {value :: [CUInt]} deriving (Show) --fc2PGRGuid a structure that 'holds' an unsigned int in C
instance Storable PGRGuid  where
  sizeOf _ = (#size fc2PGRGuid)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    --val <- replicateM 4 $ (#peek fc2PGRGuid, value) ptr
    val <- (peekArray 4 ((#ptr fc2PGRGuid, value) ptr))
    return PGRGuid {value = val}

  poke ptr (PGRGuid val)  = do
   pokeArray ((#ptr fc2PGRGuid, value)  ptr) val

data Version = Version{ mJ ::CUInt, mN :: CUInt, t :: CUInt, b :: CUInt}
instance Storable Version where
  sizeOf _ = ( #size fc2Version)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mjr <- (#peek fc2Version, major) ptr
    mnr <- (#peek fc2Version, minor) ptr
    typ <- (#peek fc2Version, type) ptr
    bld <- (#peek fc2Version, build) ptr
    return Version {mJ = mjr, mN = mnr, t = typ, b = bld}
  poke ptr (Version mjr mnr typ bld) = do 
    (#poke fc2Version, major) ptr mjr
    (#poke fc2Version, minor) ptr mnr
    (#poke fc2Version, type) ptr typ
    (#poke fc2Version, build) ptr bld


data ConfigRom = ConfigRom {nV :: CUInt, cH :: CUInt, cL :: CUInt, uS :: CUInt, uV :: CUInt, uSV :: CUInt, i0 :: CUInt, i1:: CUInt, i2:: CUInt, i3::  CUInt, key :: CChar, res:: CUInt} deriving (Show)
instance Storable ConfigRom where
  sizeOf _ = (#size fc2ConfigROM)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    n <- (#peek fc2ConfigROM, nodeVendorId) ptr
    h <- (#peek fc2ConfigROM, chipIdHi) ptr
    l <- (#peek fc2ConfigROM, chipIdLo) ptr
    s <- (#peek fc2ConfigROM, unitSpecId) ptr
    v <- (#peek fc2ConfigROM, unitSWVer) ptr
    sV <- (#peek fc2ConfigROM, unitSubSWVer) ptr
    u0 <- (#peek fc2ConfigROM, vendorUniqueInfo_0) ptr
    u1 <- (#peek fc2ConfigROM, vendorUniqueInfo_1) ptr
    u2 <- (#peek fc2ConfigROM, vendorUniqueInfo_2) ptr
    u3 <- (#peek fc2ConfigROM, vendorUniqueInfo_3) ptr
    k <- (#peek fc2ConfigROM, pszKeyword) ptr
    r <- (#peek fc2ConfigROM, reserved) ptr
    return ConfigRom { nV = n, cH = h, cL = l, uS = s, uV = v, uSV = sV, i0 = u0, i1 = u1, i2 = u2, i3 = u3, key = k, res = r}
  poke ptr (ConfigRom n h l s v sV u0 u1 u2 u3 k r) = do
    (#poke fc2ConfigROM, nodeVendorId) ptr n
    (#poke fc2ConfigROM, chipIdHi) ptr h
    (#poke fc2ConfigROM, chipIdLo) ptr l
    (#poke fc2ConfigROM, unitSpecId) ptr s
    (#poke fc2ConfigROM, unitSWVer) ptr v
    (#poke fc2ConfigROM, unitSubSWVer) ptr sV
    (#poke fc2ConfigROM, vendorUniqueInfo_0) ptr u0
    (#poke fc2ConfigROM, vendorUniqueInfo_1) ptr u1
    (#poke fc2ConfigROM, vendorUniqueInfo_2) ptr u2
    (#poke fc2ConfigROM, vendorUniqueInfo_3) ptr u3
    (#poke fc2ConfigROM, pszKeyword) ptr k
    (#poke fc2ConfigROM, reserved) ptr r
  
  
data CamInfo = CamInfo { serialNum :: CUInt
                  , iFType :: CInt --technically an enumerator
                  , colorCam :: Int
                  , modelName :: CChar
                  , vendorName :: CChar
                  , sensorInfo :: CChar
                  , sensorRes :: CChar
                  , driverName :: CChar
                  , firmwareVersion :: CChar  
                  , firmwareBuildTime :: CChar  
                  , maxBusSpeed :: CInt --technically an enumerator 
                  , bayerTileFormat :: CInt -- technically an enumerator
                  , iidcVer :: CUInt 
                  , configRom :: ConfigRom
                  , majorVersion :: CUInt  
                  , minorVersion :: CUInt  
                  , userDefName :: CChar  
                  , xmlURL1 :: CChar  
                  , xmlURL2 :: CChar  
                  , macAddress :: CUChar --technically an fc2MACAddress: unsigned char octets[6]
                  , ipAddress :: CUChar  -- technically an fc2IPAddress: unsigned char octets [4]
                  , subnetMask :: CUChar -- technically an fc2IPAddress 
                  , defaultGateway :: CUChar  -- technically an fc2IPAddress
                  , reserved :: CUInt  
                  } deriving (Show)
               
instance Storable CamInfo where
  sizeOf _ = ( #size fc2CameraInfo)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
             sN <- (#peek fc2CameraInfo, serialNumber) ptr
             iT <- (#peek fc2CameraInfo, interfaceType) ptr
             cC <- (#peek fc2CameraInfo, isColorCamera) ptr
             modN <- (#peek fc2CameraInfo, modelName) ptr
             vN <- (#peek fc2CameraInfo, vendorName) ptr
             sI <- (#peek fc2CameraInfo, sensorInfo) ptr
             sR <- (#peek fc2CameraInfo, sensorResolution) ptr
             dN <- (#peek fc2CameraInfo, driverName) ptr
             fV <- (#peek fc2CameraInfo, firmwareVersion) ptr
             fBT <- (#peek fc2CameraInfo, firmwareBuildTime) ptr
             mBS <- (#peek fc2CameraInfo, maximumBusSpeed) ptr
             bTF <- (#peek fc2CameraInfo, bayerTileFormat) ptr
             iV <-  (#peek fc2CameraInfo, iidcVer) ptr
             cR <-  (#peek fc2CameraInfo, configROM) ptr
             mjV <- (#peek fc2CameraInfo, gigEMajorVersion) ptr 
             mnV <- (#peek fc2CameraInfo, gigEMinorVersion) ptr
             uDN <- (#peek fc2CameraInfo, userDefinedName) ptr
             u1 <- (#peek fc2CameraInfo, xmlURL1) ptr
             u2 <- (#peek fc2CameraInfo, xmlURL2) ptr
             mA <- (#peek fc2CameraInfo, macAddress) ptr
             iA <- (#peek fc2CameraInfo, ipAddress) ptr
             sM <- (#peek fc2CameraInfo, subnetMask) ptr
             dG <- (#peek fc2CameraInfo, defaultGateway) ptr
             r <- (#peek fc2CameraInfo, reserved) ptr
             return CamInfo { serialNum = sN, iFType = iT, colorCam = cC, modelName = modN, vendorName = vN, sensorInfo = sI,sensorRes = sR, driverName = dN , firmwareVersion = fV, firmwareBuildTime = fBT, maxBusSpeed = mBS, bayerTileFormat = bTF, iidcVer = iV, configRom = cR, majorVersion = mjV, minorVersion = mnV , userDefName = uDN, xmlURL1 = u1, xmlURL2 = u2, macAddress = mA, ipAddress = iA, subnetMask = sM , defaultGateway = dG, reserved = r}
  poke ptr (CamInfo sN iT cC modN vN sI sR dN fV fBT mBS bTF iV cR mjV mnV uDN u1 u2 mA iA sM dG r) = do 
    (#poke fc2CameraInfo, serialNumber) ptr sN
    (#poke fc2CameraInfo, interfaceType) ptr iT
    (#poke fc2CameraInfo, isColorCamera) ptr cC
    (#poke fc2CameraInfo, modelName) ptr modN
    (#poke fc2CameraInfo, vendorName) ptr vN
    (#poke fc2CameraInfo, sensorInfo) ptr sI
    (#poke fc2CameraInfo, sensorResolution) ptr sR
    (#poke fc2CameraInfo, driverName) ptr dN
    (#poke fc2CameraInfo, firmwareVersion) ptr fV
    (#poke fc2CameraInfo, firmwareBuildTime) ptr fBT
    (#poke fc2CameraInfo, maximumBusSpeed) ptr mBS
    (#poke fc2CameraInfo, bayerTileFormat) ptr bTF
    (#poke fc2CameraInfo, iidcVer) ptr iV
    (#poke fc2CameraInfo, configROM) ptr cR
    (#poke fc2CameraInfo, gigEMajorVersion) ptr mjV 
    (#poke fc2CameraInfo, gigEMinorVersion) ptr mnV
    (#poke fc2CameraInfo, userDefinedName) ptr uDN
    (#poke fc2CameraInfo, xmlURL1) ptr u1
    (#poke fc2CameraInfo, xmlURL2) ptr u2
    (#poke fc2CameraInfo, macAddress) ptr mA
    (#poke fc2CameraInfo, ipAddress) ptr iA
    (#poke fc2CameraInfo, subnetMask) ptr sM
    (#poke fc2CameraInfo, defaultGateway) ptr dG
    (#poke fc2CameraInfo, reserved) ptr r


data CImage = CImage { rows        :: CUInt
                     , cols        :: CUInt  
                     , stride      :: CUInt  
                     , pData       :: Ptr CUChar  
                     , dataSize    :: CUInt  
                     , format      :: CInt -- an fc2PixelFormat enum
                     , bayerFormat :: CInt -- an enum
                     , imageIMPl   :: Ptr ()  
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
 
