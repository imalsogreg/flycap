module System.FlyCap where


import Foreign
import Foreign.C.Types
import Foreign.Ptr
import System.FlyCap.FlyCapBase
import Codec.Picture

hGetNum :: Context -> IO Int
hGetNum c = 
  alloca $ \ptr -> do
     fc2getNumOfCameras c ptr
     cNum <- peek ptr
     let num = fromIntegral cNum
     return (num)
     
hCreateC :: IO Context
hCreateC =
    alloca $ \pContext -> do
       fc2CreateContext pContext
       context <- peek pContext
       return (context)

hGetCamIndex :: Context -> Int -> IO PGRGuid
hGetCamIndex c i =
  alloca $ \ptr -> do
    fc2GetCameraFromIndex c (fromIntegral i) ptr
    guid <- peek ptr
    return (guid)
    
hGetCamSerial :: Context -> Int -> IO PGRGuid
hGetCamSerial c i = 
   alloca $ \ptr -> do
     fc2GetCameraFromSerialNumber c (fromIntegral i) ptr
     guid <-peek  ptr
     return (guid)

hConnect :: Context -> PGRGuid -> IO ()
hConnect c guid =
  alloca $ \pG -> do
    poke pG guid
    fc2Connect c pG
    return ()

hLibVersion :: IO Version
hLibVersion = 
  alloca $ \ptrV -> do
    fc2GetLibraryVersion ptrV
    ver <- peek ptrV
    return ver
    
hGetCamInfo :: Context -> IO CamInfo
hGetCamInfo c = 
  alloca $ \ptrCI -> do
    fc2GetCameraInfo c ptrCI
    camInfo <- peek ptrCI
    return camInfo
    
hSetVMandFR :: Context -> Int -> Int -> IO ()
hSetVMandFR c vidMode frameRate = do 
  fc2SetVideoModeAndFrameRate c (fromIntegral vidMode) (fromIntegral frameRate)
  return ()
  
hStartSCapture :: Int -> Context -> IO ()
hStartSCapture i c =
  alloca $ \ptr -> do
    poke ptr c
    fc2StartSyncCapture (fromIntegral i) ptr
    return ()

hRetrieveBuffer :: Context ->  IO CImage
hRetrieveBuffer c  =
  alloca $ \ptrImage -> do
    fc2CreateImage ptrImage
    fc2RetrieveBuffer c ptrImage
    image <- peek ptrImage
    return image
    
hStopCapture :: Context -> IO ()
hStopCapture c = do
  fc2StopCapture c
  return ()
  
hGetImageData :: CImage -> IO ImageData
hGetImageData image = 
  alloca $ \ptrID -> do 
    alloca $ \ptrImage -> do
      poke ptrImage image :: IO ()
      fc2GetImageData ptrImage ptrID
      id <- peek ptrID
      return id
      
hDisconnect :: Context -> IO()
hDisconnect c = do
  fc2Disconnect c
  return ()
  
