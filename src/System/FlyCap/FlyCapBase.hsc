{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module System.FlyCap.FlyCapBase where

import Foreign
import Foreign.C.Types
import Foreign.Ptr

#include <FlyCapture2_C.h>

-- raw c imports here

type Context = Ptr () --fc2Context is a void pointer in C
type PGRGuid = CUInt --fc2PGRGuid a structure that 'holds' an unsigned int in C
type Error = CInt
type ImageData = Ptr CUChar


data Version = { mJ ::CUInt, mN :: CUInt, t :: CUInt, b :: CUInt}
instance Storable Version where
  sizeOf _ = ( #size fc2Version)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mjr <- (#peek fc2Version, major) ptr
    mnr <- (#peek fc2Version, minor) ptr
    typ <- (#peek fc2Version, type) ptr
    bld <- (#peek fc2Version, build) ptr
    return Version {mJ = mjr, mN = mnr, t = typ, b = bld}
  poke ptr (Version mjr, mnr, typ, bld) = do 
    (#poke fc2Version, major) ptr mjr
    (#poke fc2Version, minor) ptr mnr
    (#poke fc2Version, type) ptr typ
    (#poke fc2Version, build) ptr bld


data ConfigRom = ConfigRom {nV :: CUInt, cH :: CUInt, cL :: CUInt, uS :: CUInt, uV :: CUInt, uSV :: CUInt, i0 :: CUInt, i1:: CUInt, i2:: CUInt, i3::  CUInt, key :: CChar, res:: CUInt} 
instance Storable ConfigRom where
  sizeOf _ = (#size fc2ConfigROM)
  alignment _ = alignment (undefined :: CCInt)
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
  poke ptr (ConfigRom n, h, l, s, v, sV, u0, u1, u2, u3, k, r) = do
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
                  }
               
instance Storable CamInfo where
  sizeOf _ = ( #size fc2CameraInfo)
  alignment _ = alignment (undefined :: CDouble) --??
  peek ptr = do
    sN <- (#peek fc2CameraInfo, serialNumber) ptr
    iT <- (#peek fc2CameraInfo, interfaceType) ptr
    cC <- (#peek fc2CameraInfo, isColorCamera) ptr
    mN <- (#peek fc2CameraInfo, modelName) ptr
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
    return CamInfo { serialNum = sN, iFType = iT, colorCam = cC, modelName = mN, vendorName = vN, sensorInfo = sI,sensorRes = sR, driverName = dN , firmwareVersion = fV, firmwareBuildTime = fBT, maxBusSpeed = mBS, bayerTileFormat = fBT, iidcVer = iV, configRom = cR, majorVersion = mjV, minorVersion = mnV , userDefName = uDN, xmlURL1 = u1, xmlURL2 = u2, macAddress = mA, ipAddress = iA, subnetMask = sM , defaultGateway = dG, reserved = r}
  poke ptr (CamInfo sN iT cC mN vN sI sR dN fV fBT mBS bTF iV cR mjV mnV uDN u1 u2 mA iA sM dG r) = do 
    (#poke fc2CameraInfo, serialNumber) ptr sN
    (#poke fc2CameraInfo, interfaceType) ptr iT
    (#poke fc2CameraInfo, isColorCamera) ptr cC
    (#poke fc2CameraInfo, modelName) ptr mN
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


data CImage = CImage { rows :: CUInt
                   , cols :: CUInt  
                   , stride :: CUInt  
                   , pData :: Ptr CUChar  
                   , dataSize :: CUInt  
                   , format :: CInt --technically an fc2PixelFormat (an enumerator)
                   , bayerFormat ::CInt --technically an enumerator  
                   , imageIMPl :: Ptr ()  
                   }  
              
instance Storeable CImage where
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
  poke ptr (CImage r, c, s, p ,dS, f, bF, iI) = do 
    (#poke fc2Image, rows) ptr r
    (#poke fc2Image, cols) ptr c
    (#poke fc2Image, stride) ptr s
    (#poke fc2Image, pData) ptr p
    (#poke fc2Image, dataSize) ptr dS
    (#poke fc2Image, format) ptr f
    (#poke fc2Image, bayerFormat) ptr bF
    (#poke fc2Image, imageImpl) ptr iI


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

--TODO: FINISH THE FUNCTIONS CONVERTING FROM JUICYPIXELS IMAGE TO CIMAGE

     
ctoJImage :: CImage -> IO Image
ctoJImage CImage r c _ p _ _ _ _ = do
  let h = fromIntegral r
  let w = fromIntegral c
  allocaArray $ \ptr -> do
    let ptr =p 
    dC <- peekArray ptr
    let d = map (fromIntegral) dC
    let image = Image w h d
    return image
    
  
{-

   **what should I do with these ones?
   CVCapture ??
   PrintErrorTrace ... fc2ErrorToDescription ??
 
   **do I need this function?? 
   fc2imageEventCallback
   
   **TODO:
   test/see if this works
   look for more functions to bind (like the ones in the test file)/make it work

-}