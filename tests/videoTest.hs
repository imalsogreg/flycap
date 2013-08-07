import Graphics.Rendering.OpenGL
import Control.Monad
import Control.Concurrent
import Data.IORef
import Graphics.UI.GLFW as GLFW
import System.Exit (exitWith, ExitCode (..))
--import qualified Data.Vector.Storable as VS
--import qualified Codec.Picture as JP
--import qualified Codec.Picture.Types as JPTypes
import System.FlyCap

-- only one retrieve buffer per cycle
-- forever 30 frames a second using thread delay
-- look at c api and figure out how retrieve buffer behaves
-- look at other examples (c++ flycap)

main = do
  c <- hCreateC
  cameraInit c
  -- cameraStop c -> when/how?!
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 1024 1024 "testing images" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setKeyCallback win (Just (keyPressed c))
  --GLFW.setWindowRefreshCallback win (Just display)
  GLFW.setFramebufferSizeCallback win (Just resize)
  GLFW.setWindowCloseCallback win (Just (shutdown c))
  initGL
  forever $ do
    print "System in Forever loop"
    GLFW.pollEvents
    print "just polled events"
    i <- hRetBuff c -- gives us a CImage 
    --old system using hRetrieveBuffer, which gives us an FCImage, and JuicyPixels
    -- im <-getImage  i -- gives us a JImage with Y8 Pixel
    --let image = (JPTypes.promoteImage im :: JPTypes.Image JPTypes.PixelRGB8) --change to RGB8 Pixel
    print "just retrieved image"
    tex <- loadTex i
    display tex
    GLFW.swapBuffers win
    print "just swappedBuffers"
    threadDelay 50000
 
getT :: CImage -> IO ()  
getT (CImage r c str pData dS f bF iI)  = do
  (texImage2D Nothing NoProxy 0 Luminance8 (TextureSize2D (fromIntegral c) (fromIntegral r)) 0 (PixelData Luminance UnsignedByte  pData))
  print "system at getT"
  
{- old system using juicy pixels:
  getTex :: JP.Image (JPTypes.PixelRGB8) -> IO ()
getTex (JP.Image width height dat) = do
  VS.unsafeWith dat $ \ptr -> do
    (texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (PixelData RGB UnsignedByte ptr))
  print "system at getTex"
-}                             
               
display :: TextureObject -> IO()
display tex = do
  clear [ColorBuffer]
  loadIdentity
  textureBinding Texture2D $= Just tex
  scale (1) (-1) (0 :: GLfloat)
  renderPrimitive Quads $ do -- render/draw the image
    texCoord (TexCoord2 0 (1::GLfloat))
    vertex (Vertex3 (-1) 1 (0::GLfloat))
    texCoord (TexCoord2 1 (1::GLfloat))
    vertex (Vertex3 1 1 (0::GLfloat))
    texCoord (TexCoord2 1 (0::GLfloat))
    vertex (Vertex3 (1) (-1) (0::GLfloat))
    texCoord (TexCoord2 0 (0::GLfloat))
    vertex (Vertex3 (-1) (-1) (0::GLfloat))
  flush
  print "system at display"

  
--loadTex :: JP.Image JPTypes.PixelRGB8 -> IO TextureObject (old system)
loadTex :: CImage -> IO TextureObject
loadTex im = do
  texobj <- genObjectNames 1 -- get a list of exactly 1 texture object
  textureBinding Texture2D $= Just (head texobj)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  getT im -- put the image into the texture
  print "system at loadTex"
  return $ head texobj
 
cameraInit c= do
  num <- hGetNum c
  pgrguid <- hGetCamSerial c 12320156
  hConnect c pgrguid
  info <- hGetCamInfo c
  hSetVMandFR c  VM800x600_Y8  Fr_30
  hStartCapture c

cameraStop c = do
  hStopCapture c
  hDisconnect c

resize :: GLFW.WindowSizeCallback
resize win w 0 = resize win w 1
resize _ width height = do
  viewport $=( (Position 0 0), (Size (fromIntegral width) (fromIntegral height)))
  matrixMode $= Projection 
  loadIdentity
  ortho (-1.0) 1.0 (-1.0) 1.0 (-1) (1 :: GLdouble)
  matrixMode $= Modelview 0
  loadIdentity
  flush
  
initGL :: IO ()
initGL = do
  GLFW.windowHint $ WindowHint'RedBits 8
  GLFW.windowHint $ WindowHint'GreenBits 8
  GLFW.windowHint $ WindowHint'BlueBits 8
  texture Texture2D $= Enabled
  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
  flush
  return ()

keyPressed :: Context -> GLFW.KeyCallback
keyPressed c win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown c win
keyPressed _  _ _ _ _ _ = return ()

--shutdown :: GLFW.WindowCloseCallback
shutdown c win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitWith ExitSuccess
  cameraStop c


--TODO :
-- make it show more than 10 frames at a time!!!
-- add more keys pressed options