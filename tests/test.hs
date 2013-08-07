import System.FlyCap
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPTypes
import qualified Data.Vector.Storable as VS
import Control.Concurrent
import Control.Monad

import Foreign
import Foreign.Ptr


retBuf' :: Context -> Ptr CImage -> IO ()

retBuf :: Context -> IO CImage
retBuf c = alloca $ \cImage' -> do
  fc2CreateImage cImage'
  cImage <- peek cImage'
--  print cImage'
--  print cImage
  fc2RetrieveBuffer c cImage'
  cImage2 <- peek cImage'
--  print cImage2
  --peek cImage'
 -- return $ CImage 1 1 1 nullPtr 10 0 0 nullPtr
  return cImage2


main = do
  c <- hCreateC
  num <- hGetNum c
  pgrguid <- hGetCamIndex c 0
  hConnect c pgrguid
  info <- hGetCamInfo c
  hStartCapture c 
  --iterate 11 times:
  forM_ [1..9] (\n -> retBuf c >>= \(CImage nRow nCol _ _ _ _ _ _) -> print (show n ++ ": " ++ show (nCol * nRow)) >> threadDelay 33000) 
                                 --dImage <- getDynamicImage image
 -- JP.saveBmpImage "test.bmp"  dImage
 -- test <- JP.readBitmap "test.bmp" 
 -- case test of Left s -> print s
  --             Right image -> putStrLn "success"
--  hStopCapture c
--  hDisconnect c
--  hDestroyContext c
  
   {- 
getDynamicImage :: FCImage -> IO JP.DynamicImage
getDynamicImage (FCImage nCol nRow vData) = do
  let jCData = VS.map fromIntegral vData
  let imageJ = (JP.Image nCol nRow jCData ::JPTypes.Image JPTypes.Pixel8) 
  let dImage = JP.ImageRGB8 (JPTypes.promoteImage imageJ :: JPTypes.Image JPTypes.PixelRGB8)
  return dImage
-}