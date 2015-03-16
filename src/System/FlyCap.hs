{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.FlyCap where

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.Reader.Class
import qualified Data.ByteString.Lazy as BSL
import Data.Traversable
import System.FlyCap.Internal
import qualified System.FlyCap.Internal as FCI
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

import Foreign.C.Error

import System.FlyCap.Internal hiding (Context)

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP

newtype FlyCap a = FlyCap { unFlyCap :: ReaderT Context IO a }
  deriving (MonadIO, MonadReader Context, Monad, Applicative, Functor)

type ColorImage = JP.Image JP.PixelRGBA8

data CameraID
  = CameraIndex Int
  | CameraSSN   Int
  deriving (Eq, Show)


-- * High-level requests


getFrameProducer :: CaptureOptions -> [CameraID] -> EitherT String IO (IO [ColorImage], IO ())
getFrameProducer opts@CaptureOptions{..} ids =
  traverse setupCamera opts ids
    where setupCamera :: CaptureOptions -> CameraID -> EitherT String IO (IO ColorImage, IO ())
          setupCamera (CaptureOptions (FreeRunning fps)) cID = runFlyCap $ do
            cam <- getCamera cID
            connect cam
            startCapture
            return (getFrame, stopCapture)

data CaptureTiming
  = FreeRunning Double
  | Triggered TriggerStyle
  deriving (Eq, Show)

data CaptureOptions = CaptureOptions {
    captureTiming :: CaptureTiming
  } deriving (Eq, Show)


-- * Setup

runFlyCap :: FlyCap a -> IO a
runFlyCap a = do
  ctx <- fc1 <$> fc2CreateContext
  a'  <- (runReaderT $ unFlyCap a) ctx
  fc0 <$> fc2DestroyContext ctx
  return a'

connect :: Guid -> FlyCap ()
connect g = do
  ctx <- ask
  _ <- liftIO $ fc2Connect ctx g
  return ()

startCapture :: FlyCap ()
startCapture = do
  ctx <- ask
  liftIO $ fc0 <$> fc2StartCapture ctx

stopCapture :: FlyCap ()
stopCapture = do
  ctx <- ask
  liftIO $ fc0 <$> fc2StopCapture ctx

startSyncCapture :: [Context] -> IO ()
startSyncCapture cxts = fc0 <$> fc2StartSyncCapture (length cxts) cxts


-- * Info

getCameraInfo :: FlyCap CamInfo
getCameraInfo = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetCameraInfo ctx

version :: Version
version = fc1 $ unsafePerformIO fc2GetLibraryVersion


getNumOfCameras :: FlyCap Int
getNumOfCameras = do
  ctx <- ask
  liftIO $ (fromIntegral . fc1) <$> fc2GetNumOfCameras ctx

getCamera :: CameraID -> FlyCap Guid
getCamera (CameraIndex n) = getCameraFromIndex n
getCamera (CameraSSN   s) = getCameraFromSerialNumber s

getCameraFromIndex :: Int -> FlyCap Guid
getCameraFromIndex i = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetCameraFromIndex ctx i


getCameraFromSerialNumber :: Int -> FlyCap Guid
getCameraFromSerialNumber sn = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetCameraFromSerialNumber ctx sn


-- * Frames

fc2Juicy :: FCImage -> IO ColorImage
fc2Juicy fcImg = do
  b' <- convertImageTo Fc2PixelFormatRgb8 fcImg
  dataPtr <- newForeignPtr_ (castPtr (data'FCImage b'))
  let wid = width'FCImage b'
      hgt = height'FCImage b'
      isColor = stride'FCImage b' == 3 * wid
      fromRGB = JP.imageFromUnsafePtr
      fromG   = JP.imageFromUnsafePtr
      jImg'
        | isColor = JP.imageFromUnsafePtr wid hgt dataPtr
        | otherwise = JP.promoteImage (JP.imageFromUnsafePtr wid hgt dataPtr :: JP.Image JP.Pixel8)
    in return jImg'


getFrame :: FlyCap FCImage
getFrame = do
  ctx <- ask
  img <- liftIO createImage
  img' <- retrieveBuffer img
  return img'


createImage :: IO FCImage
createImage = fc1 <$> fc2CreateImage

retrieveBuffer :: FCImage -> FlyCap FCImage
retrieveBuffer blankImg = do
  ctx <- ask
  liftIO $ fc1 <$> fc2RetrieveBuffer ctx blankImg

convertImageTo :: PixelFormat -> FCImage -> IO FCImage
convertImageTo fmt img = do
  createImage >>= \img' -> fc1 <$> fc2ConvertImageTo fmt img img'


-- * Triggers

fireSoftwareTrigger :: FlyCap ()
fireSoftwareTrigger = do
  ctx <- ask
  liftIO $ fc0 <$> fc2FireSoftwareTrigger ctx

getTriggerMode :: FlyCap TriggerMode
getTriggerMode = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetTriggerMode ctx

setTriggerMode :: TriggerMode -> FlyCap ()
setTriggerMode tm = do
  ctx <- ask
  liftIO $ fc0 <$> fc2SetTriggerMode ctx tm

getTriggerModeInfo :: FlyCap TriggerModeInfo
getTriggerModeInfo = do
  ctx <- ask
  liftIO $ fc1 <$> fc2GetTriggerModeInfo ctx


-- * Experiment / testing

testAction :: IO ()
testAction = runFlyCap $ do
  getNumOfCameras >>= (\n -> liftIO (print n))
  g <- getCameraFromIndex 0
  connect g
  getCameraInfo >>= (\i -> liftIO (print i))
  startCapture
  liftIO $ print "Started Capture"
  b <- getFrame
  liftIO $ print "Got frame"
  stopCapture

  b' <- liftIO $ convertImageTo Fc2PixelFormatRgb8 b
  dataPtr <- liftIO $ newForeignPtr_ (castPtr $ data'FCImage b')
  let wid     = width'FCImage b'
      hgt     = height'FCImage b'
      isColor = stride'FCImage b' == 3 * wid
      fromRGB = JP.imageFromUnsafePtr
      fromG   = JP.imageFromUnsafePtr
      jImg'
        | isColor   = JP.encodeBitmap (fromRGB wid hgt dataPtr :: JP.Image JP.PixelRGB8)
        | otherwise = JP.encodeBitmap (fromG   wid hgt dataPtr :: JP.Image JP.Pixel8)
  case JP.decodeImage (BSL.toStrict jImg') of
    Left s     -> liftIO . print $ "Decode error: " ++ s
    Right jImg -> liftIO $ JP.saveBmpImage "test.bmp" jImg

-- FlyCapture image specialized on CUChar (C 8-bit greyscale) pixels
-- Obviously not the optimal data type.  We'd want the option
-- to handle color / 2-byte pixels too.  Maybe depend on juicypixels
-- and use their DynamicImage type instead of this

-- type Context = FlyCapBase.Context

-- TODO: import CV, make type synonyms to clarify that Capture refers
-- to AVI files and Context refers to individual cameras
--data CaptureHandle = AVISetHandle [Capture]
--                   | CameraSetHandle [Context]


{-
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
    
-- BROKEN
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

{-
makeAVI ::Maybe Int -> Double -> String -> Context -> IO ()
--takes in (maybe) the number of frames, the frame rate, and a string that will be the avi file name
makeAVI mayb fr name c = do
  ac <- createAVIContext
  option <- makeAVIOption 30.0
  openAVI ac name option
  case mayb of 
    (Just n) -> replicateM_ n (hRetBuff c >>= \i -> appendAVI ac i >> destroyImage i)
    Nothing -> forever (hRetBuff c >>= \i -> appendAVI ac i >> destroyImage i)--while no exception?
  closeAVI ac
  destroyAVI ac


retImage :: (Maybe Context) -> (Maybe (Ptr Capture)) -> IO CImage
retImage mayCont mayCap = do
  case mayCont of Just context -> hRetBuff context
                  Nothing -> case mayCap of Just pcapture -> cvLoadImage pcapture
                                            Nothing -> print ("error, cannot retrieve images")


cvLoadImage :: Ptr Capture -> IO CImage
cvLoadImage pcapture = do
   ptrimage <- cvQueryFrame pcapture 
   imageD8 <- peek ptrimage
   let imageD32 = unsafeImageTo32F (S imageD8)
       (nRow,nCol) = getSize imageD32
   let (CArray ind0 indn n values) = copyImageToFCArray imageD32
   withForeignPtr values $ \v -> do
     return $ CImage nRow nCol 
-}
  
hRetBuff :: Context -> IO CImage
hRetBuff c =
  alloca $ \ptrImage -> do
    e <- fc2CreateImage ptrImage
    eRB <- fc2RetrieveBuffer c ptrImage
    (CImage r c str pData dS f bF iI) <- peek ptrImage
    return (CImage r c str pData dS f bF iI)

{-
hStartCapture :: Context -> IO ()
hStartCapture c = do
  throwErrnoIf_ (/=0) "Start capture failure" (fc2StartCapture c)
  
hStopCapture :: Context -> IO ()
hStopCapture c = do
  (throwErrnoIf_ (/=0) ("stop capture")
   (fc2StopCapture c))
  return ()
-}  

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
{-
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

-}

-}


fc0 :: Error -> ()
fc0 Fc2ErrorOk = ()
fc0 e          = error $ "FlyCapture2 error: " ++ show e

fc1 :: (Error,a) -> a
fc1 (Fc2ErrorOk,a) = a
fc1 (e,_)          = error $ "FlyCapture2 error: " ++ show e

fc2 :: (Error,a,b)   -> (a,b) 
fc2 (Fc2ErrorOk,a,b) = (a,b)
fc2 (e,_,_)          = error $ "FlyCapture2 error: " ++ show e

fc3 :: (Error,a,b,c) -> (a,b,c)
fc3 (Fc2ErrorOk,a,b,c) = (a,b,c)
fc3 (e,_,_,_) = error $ "FlyCapture2 error: " ++ show e
