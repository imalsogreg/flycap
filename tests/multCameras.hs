import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.IORef
import Data.Map
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import System.Exit (exitWith, ExitCode (..))
import System.FlyCap
import System.IO.Unsafe
import Prelude hiding (lookup)

data LayoutState = LayoutState {projectMatrix :: GLmatrix GLdouble, dictionary :: Map Int (GLmatrix GLdouble)}

main = do
  cs <- createContexts
  let n = length cs -- n is the number of cameras we have
  print n
  zipWithM_ cameraInit cs [0..]
  hStartSCapture n cs
  ind <- newMVar 0
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 640 (480 + 480 `div` n) "testing images" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  (texs, pMatrix) <- initGL n
  layout <- newTVarIO LayoutState {projectMatrix = pMatrix ,dictionary = empty}
  GLFW.setKeyCallback win (Just (keyPressed cs))
  GLFW.setFramebufferSizeCallback win (Just (resize layout  n))
  GLFW.setWindowCloseCallback  win (Just (shutdown cs))
  GLFW.setMouseButtonCallback win (Just (mouseClick layout n ind))
  resize layout n win 640 (480 + 480 `div` n)
  forever $ do
    GLFW.pollEvents
    zipWithM_ loadT cs texs
    i <-takeMVar ind
    putMVar ind i
    display layout texs i
    GLFW.swapBuffers win
 
loadT :: Context -> TextureObject -> IO()  
loadT c tex = do
    i <- hRetBuff c
    _ <- loadTex i tex
    destroyImage i

getT :: CImage -> IO ()  
getT (CImage r c str pData dS f bF iI)  = do
  (texImage2D Nothing NoProxy 0 Luminance8 (TextureSize2D (fromIntegral c) (fromIntegral r)) 0 (PixelData Luminance UnsignedByte  pData))
            
display :: TVar LayoutState -> [TextureObject] -> Int -> IO ()
display tvar texs i = do
  let num = length texs
      n = fromIntegral num
  clear [ColorBuffer]
  loadIdentity
  LayoutState pMatrix dictionary <- readTVarIO tvar
  zipWithM_ (displayCam num dictionary ) texs [0..]
  loadIdentity 
  translate (Vector3 0 (240/n) (0:: GLfloat))
  drawTex (-320, 320, -240, 240::GLfloat) (texs !! i)
  flush


displayCam :: Int -> Map Int (GLmatrix GLdouble) -> TextureObject -> Int -> IO ()
displayCam n dictionary tex i = do
  let (Just mMatrix) = lookup i dictionary
  matrix (Just $ Modelview 0) $=  mMatrix
  drawTex (-320, 320, -240, 240::GLfloat) tex

drawTex :: (GLfloat, GLfloat, GLfloat, GLfloat) -> TextureObject -> IO ()
drawTex (xl, xh, yl, yh) tex = do
  textureBinding Texture2D $= Just tex
  renderPrimitive Quads $ do -- render/draw the image
    texCoord (TexCoord2 0 (0::GLfloat))
    vertex (Vertex3 (xl) (yh) (0::GLfloat))
    texCoord (TexCoord2 1 (0::GLfloat))
    vertex (Vertex3 (xh) (yh) (0::GLfloat))
    texCoord (TexCoord2 1 (1::GLfloat))
    vertex (Vertex3 (xh) (yl) (0::GLfloat))
    texCoord(TexCoord2 0 (1::GLfloat))
    vertex (Vertex3 (xl) (yl) (0::GLfloat))
    
loadTex :: CImage -> TextureObject -> IO TextureObject
loadTex im tex = do
  textureBinding Texture2D $= Just (tex)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  getT im -- put the image into the texture
  return $  tex
 
cameraInit ::Context -> Int -> IO()  
cameraInit c i = do
  pgr <- hGetCamIndex c i
  hConnect c pgr
  hSetVMandFR c VM800x600_Y8  Fr_30
 
cameraStop ::Context -> IO() 
cameraStop c  = do
  hStopCapture c
  hDisconnect c
  hDestroyContext c
  
resize :: TVar LayoutState -> Int -> GLFW.WindowSizeCallback
resize tvar n win width height =
  let (compositeWidth, compositeHeight) = (640, 480 + 480/(realToFrac n))
      w = (fromIntegral width :: GLdouble)
      h = (fromIntegral height :: GLdouble)
      compositeAspect = compositeWidth / compositeHeight
      winAspect = w/h
      (coordMinX, coordMaxX, coordMinY, coordMaxY)
        | winAspect > compositeAspect = (-compositeHeight/2 *winAspect, compositeHeight/2 *winAspect,(-compositeHeight)/2, compositeHeight/2) --wide
        | winAspect < compositeAspect = (-compositeWidth/2,compositeWidth/2, (-compositeWidth)/2 /winAspect, compositeWidth/winAspect/2) --tall
        | otherwise = ((-compositeWidth)/2,compositeWidth/2,(-compositeHeight)/2, compositeHeight/2)
  in do
    loadIdentity
    dictionary' <- foldM (makeDictionary n) empty [0..n-1] 
    viewport $= ((Position 0 0), (Size (fromIntegral width)((fromIntegral height) :: GLsizei)))
    matrixMode $= Projection
    loadIdentity
    ortho coordMinX coordMaxX coordMinY coordMaxY (-1) (1 :: GLdouble)
    pMatrix <- (get $ matrix (Just Projection) :: IO (GLmatrix GLdouble))
    matrixMode $= Modelview 0
    loadIdentity
    atomically $ writeTVar tvar (LayoutState {projectMatrix = pMatrix, dictionary = dictionary'})
    flush

makeDictionary ::  Int -> Map Int (GLmatrix GLdouble) -> Int -> IO (Map Int (GLmatrix GLdouble))
makeDictionary num dictionary i = do
  let n = realToFrac num
  translate (Vector3 (-320 + 320/n + 640/n*(fromIntegral i)) (-240) (0::GLfloat))
  scale (1/n) (1/n) (1::GLfloat)
  matrix <- (get $ matrix (Just $ Modelview 0) :: IO (GLmatrix GLdouble))
  loadIdentity
  return $ insert i matrix dictionary
  
  
initGL :: Int -> IO ([TextureObject], GLmatrix GLdouble)
initGL num = do
  GLFW.windowHint $ WindowHint'RedBits 8
  GLFW.windowHint $ WindowHint'GreenBits 8
  GLFW.windowHint $ WindowHint'BlueBits 8
  texture Texture2D $= Enabled
  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
  texs <- genObjectNames (num)
  pMatrix <- (get $ matrix (Just Projection) :: IO (GLmatrix GLdouble))
  flush
  return (texs, pMatrix)

mouseClick :: (TVar LayoutState) -> Int -> MVar Int -> GLFW.MouseButtonCallback
mouseClick tvar num ind win  (MouseButton'1)  (MouseButtonState'Released) (ModifierKeys False False False False) = do
  (x,y) <- getWindowSize win
  (mX, mY) <- getCursorPos win
  LayoutState pMatrix dictionary' <- readTVarIO tvar
  vport <- get viewport
  let  newDictionary = (Data.Map.map (\mV -> unProject (Vertex3 (realToFrac mX) (realToFrac mY) (0.0::GLdouble)) mV pMatrix vport) dictionary' :: Map Int (IO (Vertex3 GLdouble)))
  mapM_ (\(k,v) -> setCurrent k ind v) (assocs newDictionary)
  flush
mouseClick _  _ _ _ _ _ _ = return ()

setCurrent :: Int -> MVar Int -> IO (Vertex3 GLdouble) -> IO ()
setCurrent i mvar v3 = do
  (Vertex3 x y z) <- v3
  print $ unwords [show x, show y, show z]
  if( x <= 320 && x >= -320 && y <= 1200 && y >= 725) then do takeMVar mvar
                                                              putMVar mvar i
    else return ()

keyPressed :: [Context] -> GLFW.KeyCallback
keyPressed cs win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown cs win
keyPressed _  _ _ _ _ _ = return ()

shutdown :: [Context] -> GLFW.WindowCloseCallback
shutdown cs win = do
  mapM_ cameraStop cs
  GLFW.destroyWindow win
  GLFW.terminate
  exitWith ExitSuccess

--fix y values from mouseclick