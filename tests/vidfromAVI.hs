module Main where

import Control.Monad
import CV.HighGUI
import CV.Image
import CV.Video
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Storable

main :: IO () 
main = do
  let name = "test window"
  cname <- newCString name
  _ <- mkWin'_ cname (fromIntegral 1)
  let video = "/home/sarah/haskell/hFlyCapture/tests/testingavi-0000.avi"
  cvideo <- newCString video
  pcapture <- cvCreateFileCapture cvideo 
  forM [1..30] $ \i -> do
      image <- (creatingImage $ cvQueryFrame pcapture :: IO (Image GrayScale D8))
      print $ getPixel (100,100) image
      showImage name  image
      _ <- cvWaitKey (fromIntegral 33)
      return ()
  destroyWindow name
  return ()
  

{-
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui

main :: IO ()
main = do
  
  w        <- titledWindow "Test Window" True
  _ <- waitKey 1000
  return ()
  capture' <- cvCreateFileCapture "/home/sarah/Desktop/vid5.avi"

  forM [1..30] $ \i -> do
    img' <- cvQueryFrame capture'
    print img'
    showImage (fromIntegral w) img'
  
    _ <- waitKey 33
    return ()

  _ <- waitKey 0

  cvReleaseCapture capture'
  delWindow (fromIntegral w)
  
  return ()
-}