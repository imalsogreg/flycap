module System.FlyCap where

import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.C.Error
import System.FlyCap.FlyCapBase
import qualified Data.Vector.Storable as VS
import qualified Codec.Picture as JP

-- FlyCapture image specialized on CUChar (C 8-bit greyscale) pixels
-- Obviously not the optimal data type.  We'd want the option
-- to handle color / 2-byte pixels too.  Maybe depend on juicypixels
-- and use their DynamicImage type instead of this
data FCImage = FCImage
               Int -- ^ column count (width)
               Int -- ^ row count    (height)
               (VS.Vector CUChar)

hGetNum :: Context -> IO Int
hGetNum c = 
  alloca $ \ptr -> do
     _ <- fc2getNumOfCameras c ptr
     cNum <- peek ptr
     let num = fromIntegral cNum
     return (num)
     
hCreateC :: IO Context
hCreateC =
    alloca $ \pContext -> do
      (throwErrnoIf_ (/=0) ("create context did not work")
       (fc2CreateContext pContext))
      peek pContext

hGetCamIndex :: Context -> Int -> IO PGRGuid
hGetCamIndex c i =
  alloca $ \ptr -> do
    (throwErrnoIf_ (/=0) ("get camera index")
     (fc2GetCameraFromIndex c (fromIntegral i) ptr))
    guid <- peek ptr
    return (guid)
    
hGetCamSerial :: Context -> Int -> IO PGRGuid
hGetCamSerial c i = 
   alloca $ \ptr -> do
     _ <- fc2GetCameraFromSerialNumber c (fromIntegral i) ptr
     guid <-peek  ptr
     return (guid)

hConnect :: Context -> PGRGuid -> IO ()
hConnect c guid =
  alloca $ \pG -> do
    poke pG guid
    (throwErrnoIf_ (/=0) ("connect")
     (fc2Connect c pG))

    return ()

hLibVersion :: IO Version
hLibVersion = 
  alloca $ \ptrV -> do
    _ <- fc2GetLibraryVersion ptrV
    ver <- peek ptrV
    return ver
    
hGetCamInfo :: Context -> IO CamInfo
hGetCamInfo c = 
  alloca $ \ptrCI -> do
    (throwErrnoIf_ (/=0) ("get camera info")
     (fc2GetCameraInfo c ptrCI))
    camInfo <- peek ptrCI
    return camInfo
    
hSetVMandFR :: Context -> Int -> Int -> IO ()
hSetVMandFR c vidMode frameRate = do 
  _ <- fc2SetVideoModeAndFrameRate c (fromIntegral vidMode) (fromIntegral frameRate)
  return ()
  
hStartSCapture :: Int -> Context -> IO ()
hStartSCapture i c =
  alloca $ \ptr -> do
    poke ptr c
    (throwErrnoIf_ (/=0) ("start capture")
     (fc2StartSyncCapture (fromIntegral i) ptr))
    return ()

hRetrieveBuffer :: Context -> IO FCImage
hRetrieveBuffer c  =
  alloca $ \ptrImage -> do
    (throwErrnoIf_ (/=0) ("creating image -- for retrieving buffer")
     (fc2CreateImage ptrImage))
    (throwErrnoIf_ (/=0) ("retrieve buffer")
     (fc2RetrieveBuffer c ptrImage))
    cImage <- peek ptrImage
    cImageDataF <- newForeignPtr_ (pData cImage)
    let nR = fromIntegral $ rows cImage
        nC = fromIntegral $ cols cImage
    return $
      FCImage nC nR (VS.unsafeFromForeignPtr cImageDataF 0 (nC * nR))

hStartCapture :: Context -> IO ()
hStartCapture c = do
  error <- fc2StartCapture c
  return ()
  
hStopCapture :: Context -> IO ()
hStopCapture c = do
  (throwErrnoIf_ (/=0) ("stop capture")
   (fc2StopCapture c))
  return ()
  
hGetImageData :: CImage -> IO ImageData
hGetImageData image = 
  alloca $ \ptrID -> do 
    alloca $ \ptrImage -> do
      poke ptrImage image :: IO ()
      _ <- fc2GetImageData ptrImage ptrID
      ptrid <- peek ptrID
      return ptrid
      
hDisconnect :: Context -> IO()
hDisconnect c = do
  (throwErrnoIf_ (/=0) ("disconnecting")
   (fc2Disconnect c))
  return ()
