import Foreign.C.Types
import Foreign.Ptr
import System.FlyCap
import Control.Monad

main = do 
 {- c <- hCreateC
  pgr <- hGetCamIndex c 0
  hConnect c pgr
  hSetVMandFR c  VM800x600_Y8  Fr_30
  hStartCapture c
  ac <- createAVIContext --this is working
  option <- makeAVIOption 30.0
  openAVI ac "testingavi.avi" option
  replicateM_ 20  (hRetBuff c >>= \i -> appendAVI ac i >> destroyImage i)
  closeAVI ac
  destroyAVI ac
  hStopCapture c
  hDisconnect c
  hDestroyContext c
-}
  fromAVI
  return ()