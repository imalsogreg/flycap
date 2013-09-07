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
                     , createContexts  
                     , createMore
                     , destroyAVI
                     , createAVIContext
                     , makeAVIOption
                     , openAVI
                     , closeAVI  
                     , appendAVI  
                     , fromAVI 
                     ) where

import qualified System.FlyCap.FlyCapBase as FlyCapBase
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr.Safe
import Foreign.C.Error
import Control.Monad
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui
import System.FlyCap.FlyCapBase hiding (Context)
import qualified Data.Vector.Storable as VS
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPTypes

-- FlyCapture image specialized on CUChar (C 8-bit greyscale) pixels
-- Obviously not the optimal data type.  We'd want the option
-- to handle color / 2-byte pixels too.  Maybe depend on juicypixels
-- and use their DynamicImage type instead of this

type Context = FlyCapBase.Context

data CaptureHandle = AVISetHandle [Capture]
                     | CameraSetHandle [Context]

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
    
hSetVMandFR :: Context -> VideoMode -> FrameRate -> IO ()
hSetVMandFR c vidMode frameRate = do 
  _ <- fc2SetVideoModeAndFrameRate c (vmToC vidMode) (frToC frameRate)
  return ()
  
hStartSCapture :: Int -> [Context] -> IO ()
hStartSCapture i c =
  allocaArray i $ \ptr -> do
    pokeArray ptr c
    e <- fc2StartSyncCapture (fromIntegral i) ptr
    if i == 0 then return ()
      else print $ "error from start sync capture is: " ++ show e

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

makeAVI ::Maybe Int -> Double -> String -> Context -> IO ()
--takes in (maybe) the number of frames, the frame rate, and a string that will be the avi file name
--should I have it create a context and not take in one?
makeAVI mayb fr name c = do
  ac <- createAVIContext
  option <- makeAVIOption 30.0
  openAVI ac name option
  case mayb of 
    (Just n) -> replicateM_ n (hRetBuff c >>= \i -> appendAVI ac i >> destroyImage i)
    Nothing -> forever (hRetBuff c >>= \i -> appendAVI ac i >> destroyImage i)--while no exception?
  closeAVI ac
  destroyAVI ac

getImage :: (Maybe Context) -> (Maybe (Ptr CvCapture)) -> IO CImage
getImage mayCont mayCap = do
  case mayCont of Just context -> hRetBuff context
                  Nothing -> 
                    case mayCap of Just pcapture -> do
                                     ptrimage <- cvQueryFrame pcapture 
                                     (IplImage ) <- peek ptrimage
                                     

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

createContexts :: IO [Context]
createContexts = do
  c <- (hCreateC :: IO Context)
  n <- hGetNum c
  let l = (c:[])
  createMore n 1 l
  
createMore :: Int -> Int -> [Context] -> IO [Context]
createMore n l c = do
  if n == l then return c
    else do
      cNew <- hCreateC
      createMore n (l+1) (cNew:c)
      
appendAVI :: AVIContext -> CImage -> IO ()
appendAVI c image = do
  alloca $ \ptr -> do
    poke ptr image
    e <- fc2AVIAppend c ptr
    if e == 0 then return ()
      else print $ "error from append AVI is: " ++ (show e)

closeAVI :: AVIContext -> IO ()
closeAVI c = do
  e <- fc2AVIClose c
  if e == 0 then return ()
    else print ("Error for Close AVI is: " ++ show e)

openAVI :: AVIContext -> String -> AVIOption -> IO ()
openAVI c n o = do
  cstring <- newCString n
  alloca $ \optPtr -> do
    poke optPtr o
    e  <- fc2AVIOpen c cstring optPtr
    if e == 0 then return ()
      else print $ "Error for Open AVI is: " ++ (show e)
  
makeAVIOption :: Double  -> IO AVIOption
makeAVIOption fr = do
  let cFR = (realToFrac fr :: CFloat)
  ptr <- mallocArray 256 
  return AVIOption {frameRate = cFR, reservedList = ptr}
  
createAVIContext :: IO AVIContext
createAVIContext = 
  alloca $ \ptr -> do
    e <- fc2CreateAVI ptr
    print $ "error from avi context is: " ++ (show e)
    peek ptr
           
destroyAVI :: AVIContext -> IO ()
destroyAVI c = do
  e <- fc2DestroyAVI c
  if e == 0 then return ()
    else print $ "Error for Destroy AVI is: " ++ (show e)
         
         
-- continue this, only use opengl now
fromAVI = do 
  makeWindow "testing"
  cap <- captureFromFile "testingavi-0000.avi"
  forM [1..20] $ \i -> do
    Just im <- getFrame cap
    showImage "newim" im
    waitKey (fromIntegral 5)
    return ()
  waitKey 0
  destroyWindow "testing"