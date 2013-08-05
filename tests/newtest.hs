import Graphics.Rendering.OpenGL
import Codec.Picture
import Codec.Picture.Types
import qualified Data.ByteString.Lazy as B
import Graphics.UI.GLUT
import Foreign
import Data.Vector.Storable (unsafeWith)
import Control.Monad
import Control.Concurrent
import Data.IORef

main = do
  _ <- getArgsAndInitialize
  createWindow "test" 
  initGL
  texs <- loadTex "test.bmp" "2red2green.bmp"
  tex <- newIORef (cycle texs)
  displayCallback $= display tex
  idleCallback $= Just (display tex)
  mainLoop 

getTex fn = do
  im <- readBitmap fn
  case im of Left s -> do 
               print "read bitmap error"
               print s
             Right image -> do 
               case image of {-(ImageRGBA8 (Image width height dat)) -> do
                               unsafeWith dat $ \ptr -> do
                                                 (texImage2D Nothing NoProxy 0 RGBA8 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (PixelData RGBA UnsignedByte ptr))
                                                 
                             (ImageYA8 (Image width height dat)) -> do
                               unsafeWith dat $ \ptr -> do
                               (texImage2D Nothing NoProxy 0 Luminance8Alpha8 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (PixelData LuminanceAlpha UnsignedByte ptr))
                               
                             (ImageRGB16 (Image width height dat)) -> do
                               unsafeWith dat $ \ptr -> do
                                                 (texImage2D Nothing NoProxy 0 RGB16 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (PixelData RGB UnsignedByte ptr))
                           -}
                             (ImageRGB8 (Image width height dat)) -> do
                               unsafeWith dat $ \ptr -> do
                                                 (texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (PixelData RGB UnsignedByte ptr))
                             
               
display :: IORef [ TextureObject] -> IO()
display t' = do
  (tex:texs) <- get t'
  clear [ColorBuffer]
  loadIdentity
  textureBinding Texture2D $= Just tex
  scale (-1) (-1) (0 :: GLfloat)
  renderPrimitive Quads $ do
    texCoord (TexCoord2 0 (1::GLfloat))
    vertex (Vertex3 (-1) 1 (0::GLfloat))
    texCoord (TexCoord2 1 (1::GLfloat))
    vertex (Vertex3 1 1 (0::GLfloat))
    texCoord (TexCoord2 1 (0::GLfloat))
    vertex (Vertex3 (1) (-1) (0::GLfloat))
    texCoord (TexCoord2 0 (0::GLfloat))
    vertex (Vertex3 (-1) (-1) (0::GLfloat))
    
  t' $= texs
  flush
  threadDelay (500000)
  
{-  threadDelay (500000)
  clear [ColorBuffer]
  loadIdentity
  textureBinding Texture2D $= Just (last texs)
  renderPrimitive Quads $ do
    texCoord (TexCoord2 0 (0::GLfloat))
    vertex (Vertex3 (-1) (-1) (0::GLfloat)) 
    texCoord (TexCoord2 1 (0::GLfloat))
    vertex (Vertex3 1 (-1) (0::GLfloat))
    texCoord (TexCoord2 1 (1::GLfloat))
    vertex (Vertex3 1 1 (0::GLfloat))
    texCoord (TexCoord2 0 (1::GLfloat))
    vertex (Vertex3 (-1) 1 (0::GLfloat))
  flush
-}
  --swapBuffers
  
initGL :: IO ()
initGL = do
  texture Texture2D $= Enabled
  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
  flush
  return ()
  
loadTex :: String -> String -> IO [TextureObject]
loadTex fn1 fn2 = do
  texobjs <- genObjectNames 2
  textureBinding Texture2D $= Just (head texobjs)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  getTex fn1
  textureBinding Texture2D $= Just (last texobjs)
  textureFilter Texture2D $= ((Nearest, Nothing),Nearest)
  getTex fn2
  return texobjs
  
  
-- colors not accurate  
-- depth buffer
-- change to glfw
  
  
-- streaming 