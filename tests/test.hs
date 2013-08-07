import System.FlyCap
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPTypes
import qualified Data.Vector.Storable as VS
import Control.Concurrent
import Control.Monad

main = do
  c <- hCreateC
  num <- hGetNum c
  pgrguid <- hGetCamIndex c 0
  hConnect c pgrguid
  info <- hGetCamInfo c
  hStartCapture c
  threadDelay 1000000  
  --iterate 11 times:
  sequence $ take 12 $ repeat ( hRetrieveBuffer c >>= \(FCImage nCol nRow vData) -> print (nCol * nRow) >> threadDelay 33000) 
                                 --dImage <- getDynamicImage image
 -- JP.saveBmpImage "test.bmp"  dImage
 -- test <- JP.readBitmap "test.bmp" 
 -- case test of Left s -> print s
  --             Right image -> putStrLn "success"
  hStopCapture c
  hDisconnect c
  return ()
  
   {- 
getDynamicImage :: FCImage -> IO JP.DynamicImage
getDynamicImage (FCImage nCol nRow vData) = do
  let jCData = VS.map fromIntegral vData
  let imageJ = (JP.Image nCol nRow jCData ::JPTypes.Image JPTypes.Pixel8) 
  let dImage = JP.ImageRGB8 (JPTypes.promoteImage imageJ :: JPTypes.Image JPTypes.PixelRGB8)
  return dImage
-}