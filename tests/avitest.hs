import Foreign.C.Types
import Foreign.Ptr
import System.FlyCap
import Control.Monad

-- I think we have to delete the image each time, after we append it to our avi using destroy image in order to prevent the freezing/only going to 10

main = do 
  c <- hCreateC
  pgr <- hGetCamIndex c 0
  hConnect c pgr
  hSetVMandFR c  VM800x600_Y8  Fr_30
  hStartCapture c
  ac <- createAVIContext --this is working
  option <- makeAVIOption 30.0
  openAVI ac "testingavi.avi" option
  replicateM_ 20 (hRetBuff c >>= appendAVI ac)
  {-closeAVI ac
  destroyAVI ac
  -}
  hStopCapture c
  hDisconnect c
  hDestroyContext c
  return ()