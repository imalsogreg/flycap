import System.FlyCap
import qualified Codec.Picture as JP
import qualified Data.Vector.Storable as VS
import Control.Concurrent

main = do
  c <- hCreateC
  print $ "context is: " ++ show c
  num <- hGetNum c
  pgrguid <- hGetCamSerial c 12320156
  print $ "pgrguid" ++ show pgrguid
  threadDelay 1000000
  hConnect c pgrguid
  threadDelay 1000000
  info <- hGetCamInfo c
  print $ "info from hgetcaminfo: " ++ show info
  print $ "number of cameras: " ++ show num
  hStartCapture c
  (FCImage nCol nRow vData) <- hRetrieveBuffer c
  let jCData = VS.map fromIntegral vData
  let imageJ = JP.Image nCol nRow jCData
  let dImage = JP.ImageY8 imageJ
  JP.saveBmpImage "test.bmp" dImage
  return ()
    