module System.FlyCap ( VideoMode (..) 
                     , FrameRate (..)
                     , Context
                     , FCImage (..) 
                     , CImage (..) 
                     , hRetBuff  
                     , hGetNum 
                     , hCreateC 
                     , hGetCamIndex 
                     , hGetCamSerial 
                     , hConnect
                     , hLibVersion
                     , hGetCamInfo
                     , hSetVMandFR
                     , hStartSCapture
                     , hRetrieveBuffer
                     , hStartCapture
                     , hStopCapture
                     , hGetImageData
                     , hDisconnect
                     , getDynamicImage
                     , getImage
                     , hDestroyContext
                     , fc2CreateImage
                     , fc2RetrieveBuffer
                     , destroyImage  
                     ) where

import qualified System.FlyCap.FlyCapBase as FlyCapBase
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.C.Error
import System.FlyCap.FlyCapBase hiding (Context)
import qualified Data.Vector.Storable as VS
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPTypes

-- FlyCapture image specialized on CUChar (C 8-bit greyscale) pixels
-- Obviously not the optimal data type.  We'd want the option
-- to handle color / 2-byte pixels too.  Maybe depend on juicypixels
-- and use their DynamicImage type instead of this

type Context = FlyCapBase.Context

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
  alloca $ \ptr -> (throwErrnoIf_ (/=0) ("get camera index")
                    (fc2GetCameraFromIndex c (fromIntegral i) ptr)) >> peek ptr
    
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
    
hSetVMandFR :: Context -> VideoMode -> FrameRate -> IO ()
hSetVMandFR c vidMode frameRate = do 
  _ <- fc2SetVideoModeAndFrameRate c (vmToC vidMode) (frToC frameRate)
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
    print "created image for retrieving buffer" 
    print $ show c
    print "now trying to retrieve buffer"
    e <- fc2RetrieveBuffer c ptrImage
    print $ "error is: " ++ (show e)
   -- (throwErrnoIf_ (/=0) ("retrieve buffer")
     --(fc2RetrieveBuffer c ptrImage))
    print "retrieved image, trying to peek to pointer"
    cImage <- peek ptrImage -- CImage
    print "got cimage"
    cImageDataF <- newForeignPtr_ (pData cImage)
    print "did weird foreign pointer thing"
    let nR = fromIntegral $ rows cImage
        nC = fromIntegral $ cols cImage
    return $
      FCImage nC nR (VS.unsafeFromForeignPtr cImageDataF 0 (nC * nR))

hRetBuff :: Context -> IO CImage
hRetBuff c =
  alloca $ \ptrImage -> do
    e <- fc2CreateImage ptrImage
    eRB <- fc2RetrieveBuffer c ptrImage
    (CImage r c str pData dS f bF iI) <- peek ptrImage
    return (CImage r c str pData dS f bF iI)

hStartCapture :: Context -> IO ()
hStartCapture c = do
  throwErrnoIf_ (/=0) "Start capture failure" (fc2StartCapture c)
  
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
      id <- peek ptrID
      return id
      
hDisconnect :: Context -> IO()
hDisconnect c = do
  (throwErrnoIf_ (/=0) ("disconnecting")
   (fc2Disconnect c))
  return ()

hDestroyContext :: Context -> IO ()
hDestroyContext c =
  throwErrnoIf_ (/=0) "Destroy context failed." (fc2DestroyContext c)
    
getDynamicImage :: FCImage -> IO JP.DynamicImage
getDynamicImage (FCImage nCol nRow vData) = do
  let jCData = VS.map fromIntegral vData
  let imageJ = (JP.Image nCol nRow jCData ::JPTypes.Image JPTypes.Pixel8) 
  let dImage = JP.ImageRGB8 (JPTypes.promoteImage imageJ :: JPTypes.Image JPTypes.PixelRGB8)
  return dImage

   
getImage :: FCImage -> IO (JP.Image JPTypes.Pixel8)
getImage (FCImage nCol nRow vData) = do
  let jCData = VS.map fromIntegral vData
  let imageJ = (JP.Image nCol nRow jCData ::JPTypes.Image JPTypes.Pixel8) 
  return imageJ
  
destroyImage :: CImage -> IO ()
destroyImage im = do
  alloca $ \ptr -> do 
    poke ptr im
    e <-fc2DestroyImage ptr
    if e == 0 then return () else
                                  print $ "error from destroying image is: " ++ (show e)
                                  --return ()
