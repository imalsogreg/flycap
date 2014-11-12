{-# LANGUAGE ForeignFunctionInterface #-}

module System.FlyCap.AVI where

{-
import Foreign
import Foreign.C.Types
import Foreign.Ptr

#include <FlyCapture2_C.h>


type AVIContext = Ptr ()

data  AVIOption = AVIOption {frameRate :: CFloat, reservedList :: Ptr CUInt} 

instance Storable AVIOption where
  sizeOf _ = (#size fc2AVIOption)
  alignment _ = alignment (undefined :: CFloat)
  peek ptr = do
    fr <- (#peek fc2AVIOption, frameRate) ptr
    res <- (#peek fc2AVIOption, reserved) ptr
    return AVIOption {frameRate = fr, reservedList = res}
  poke ptr (AVIOption fr res) = do
    (#poke fc2AVIOption, frameRate) ptr fr
    (#poke fc2AVIOption, reserved) ptr res
-}
