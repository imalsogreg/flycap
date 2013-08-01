import System.FlyCap
import qualified Codec.Picture as JP
import qualified Data.Vector.Storable as VS

main = do
  c <- hCreateC
  print $ "context is: " ++ show c
  num <- hGetNum c
  pgrguid <- hGetCamSerial c 12320156
  print $ "pgrguid" ++ show pgrguid
  hConnect c pgrguid
 
  info <- hGetCamInfo c
  print $ "info from hgetcaminfo: " ++ show info
  print $ "number of cameras: " ++ show num
  hStartSCapture num c
  (FCImage nCol nRow vData) <- hRetrieveBuffer c
  let imageJ = JP.Image nCol nRow vData
  
  return ()
    