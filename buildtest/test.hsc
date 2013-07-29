{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main where

import Foreign
import Foreign.C.Types
import Foreign.Ptr

#include <FlyCapture2_C.h>

main :: IO()
main = do
  print "Hi"
  context <- hCreateC
  print context
  num <- hGetNum context
  print (num)
  return ()

foreign import ccall unsafe "FlyCapture2_C.h fc2GetNumOfCameras"
  fc2getNumOfCameras :: Context -> Ptr CUInt -> IO CInt

hGetNum :: Context -> IO Int
hGetNum c = 
  alloca $ \ptr -> do
     fc2getNumOfCameras c ptr --value that returns from here is the error message
     cNum <- peek ptr
     let num = fromIntegral cNum
     return (num)

foreign import ccall unsafe "FlyCapture2_C.h fc2CreateContext"
  fc2CreateContext :: Ptr (Context) -> IO CInt
                        
hCreateC :: IO Context
hCreateC = do
  print "Hi"
  alloca $ \pContext -> do
    fc2CreateContext pContext
    context <- peek pContext
    return (context)

type Context = Ptr ()
