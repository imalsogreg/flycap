import System.FlyCap
import Codec.Picture

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
  image <- hRetrieveBuffer c
  hStopCapture c
  hDisconnect c
  print $ "bytestring:" ++ show image
  -- savePngImage "testimage.png" image
  return ()
    
    --getpicture, save it as png via juicy pixels