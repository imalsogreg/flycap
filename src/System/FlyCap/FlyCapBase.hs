{-# LINE 1 "FlyCapBase.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# LINE 2 "FlyCapBase.hsc" #-}

module System.FlyCap.FlyCapBase where

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Codec.Picture
import qualified Data.ByteString as B
import Control.Monad


{-# LINE 13 "FlyCapBase.hsc" #-}

-- raw c imports here

type Context = Ptr () --fc2Context is a void pointer in C
type Error = CInt
type ImageData = Ptr CUChar
type AVIContext = Ptr ()
data  AVIOption = AVIOption {frameRate :: CFloat, reservedList :: Ptr CUInt} 
instance Storable AVIOption where
  sizeOf _ = ((1028))
{-# LINE 23 "FlyCapBase.hsc" #-}
  alignment _ = alignment (undefined :: CFloat)
  peek ptr = do
    fr <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 26 "FlyCapBase.hsc" #-}
    res <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 27 "FlyCapBase.hsc" #-}
    return AVIOption {frameRate = fr, reservedList = res}
  poke ptr (AVIOption fr res) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr fr
{-# LINE 30 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr res
{-# LINE 31 "FlyCapBase.hsc" #-}
    
data PGRGuid = PGRGuid {value :: [CUInt]} deriving (Show) --fc2PGRGuid a structure that 'holds' an unsigned int in C
instance Storable PGRGuid  where
  sizeOf _ = ((16))
{-# LINE 35 "FlyCapBase.hsc" #-}
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    --val <- replicateM 4 $ (#peek fc2PGRGuid, value) ptr
    val <- (peekArray 4 (((\hsc_ptr -> hsc_ptr `plusPtr` 0)) ptr))
{-# LINE 39 "FlyCapBase.hsc" #-}
    return PGRGuid {value = val}

  poke ptr (PGRGuid val)  = do
   pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 0))  ptr) val
{-# LINE 43 "FlyCapBase.hsc" #-}

data Version = Version{ mJ ::CUInt, mN :: CUInt, t :: CUInt, b :: CUInt}
instance Storable Version where
  sizeOf _ = ( (16))
{-# LINE 47 "FlyCapBase.hsc" #-}
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mjr <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 50 "FlyCapBase.hsc" #-}
    mnr <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 51 "FlyCapBase.hsc" #-}
    typ <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 52 "FlyCapBase.hsc" #-}
    bld <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 53 "FlyCapBase.hsc" #-}
    return Version {mJ = mjr, mN = mnr, t = typ, b = bld}
  poke ptr (Version mjr mnr typ bld) = do 
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr mjr
{-# LINE 56 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr mnr
{-# LINE 57 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr typ
{-# LINE 58 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr bld
{-# LINE 59 "FlyCapBase.hsc" #-}


data ConfigRom = ConfigRom {nV :: CUInt, cH :: CUInt, cL :: CUInt, uS :: CUInt, uV :: CUInt, uSV :: CUInt, i0 :: CUInt, i1:: CUInt, i2:: CUInt, i3::  CUInt, key :: CChar, res:: CUInt} deriving (Show)
instance Storable ConfigRom where
  sizeOf _ = ((616))
{-# LINE 64 "FlyCapBase.hsc" #-}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    n <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 67 "FlyCapBase.hsc" #-}
    h <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 68 "FlyCapBase.hsc" #-}
    l <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 69 "FlyCapBase.hsc" #-}
    s <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 70 "FlyCapBase.hsc" #-}
    v <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 71 "FlyCapBase.hsc" #-}
    sV <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 72 "FlyCapBase.hsc" #-}
    u0 <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 73 "FlyCapBase.hsc" #-}
    u1 <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 74 "FlyCapBase.hsc" #-}
    u2 <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 75 "FlyCapBase.hsc" #-}
    u3 <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 76 "FlyCapBase.hsc" #-}
    k <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 77 "FlyCapBase.hsc" #-}
    r <- ((\hsc_ptr -> peekByteOff hsc_ptr 552)) ptr
{-# LINE 78 "FlyCapBase.hsc" #-}
    return ConfigRom { nV = n, cH = h, cL = l, uS = s, uV = v, uSV = sV, i0 = u0, i1 = u1, i2 = u2, i3 = u3, key = k, res = r}
  poke ptr (ConfigRom n h l s v sV u0 u1 u2 u3 k r) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr n
{-# LINE 81 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr h
{-# LINE 82 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr l
{-# LINE 83 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr s
{-# LINE 84 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr v
{-# LINE 85 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr sV
{-# LINE 86 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr u0
{-# LINE 87 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr u1
{-# LINE 88 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr u2
{-# LINE 89 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr u3
{-# LINE 90 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr k
{-# LINE 91 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 552)) ptr r
{-# LINE 92 "FlyCapBase.hsc" #-}
  
  
data CamInfo = CamInfo { serialNum :: CUInt
                  , iFType :: CInt --technically an enumerator
                  , colorCam :: Int
                  , modelName :: CChar
                  , vendorName :: CChar
                  , sensorInfo :: CChar
                  , sensorRes :: CChar
                  , driverName :: CChar
                  , firmwareVersion :: CChar  
                  , firmwareBuildTime :: CChar  
                  , maxBusSpeed :: CInt --technically an enumerator 
                  , bayerTileFormat :: CInt -- technically an enumerator
                  , iidcVer :: CUInt 
                  , configRom :: ConfigRom
                  , majorVersion :: CUInt  
                  , minorVersion :: CUInt  
                  , userDefName :: CChar  
                  , xmlURL1 :: CChar  
                  , xmlURL2 :: CChar  
                  , macAddress :: CUChar --technically an fc2MACAddress: unsigned char octets[6]
                  , ipAddress :: CUChar  -- technically an fc2IPAddress: unsigned char octets [4]
                  , subnetMask :: CUChar -- technically an fc2IPAddress 
                  , defaultGateway :: CUChar  -- technically an fc2IPAddress
                  , reserved :: CUInt  
                  } deriving (Show)
               
instance Storable CamInfo where
  sizeOf _ = ( (5876))
{-# LINE 122 "FlyCapBase.hsc" #-}
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
             sN <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 125 "FlyCapBase.hsc" #-}
             iT <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 126 "FlyCapBase.hsc" #-}
             cC <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 127 "FlyCapBase.hsc" #-}
             modN <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 128 "FlyCapBase.hsc" #-}
             vN <- ((\hsc_ptr -> peekByteOff hsc_ptr 528)) ptr
{-# LINE 129 "FlyCapBase.hsc" #-}
             sI <- ((\hsc_ptr -> peekByteOff hsc_ptr 1040)) ptr
{-# LINE 130 "FlyCapBase.hsc" #-}
             sR <- ((\hsc_ptr -> peekByteOff hsc_ptr 1552)) ptr
{-# LINE 131 "FlyCapBase.hsc" #-}
             dN <- ((\hsc_ptr -> peekByteOff hsc_ptr 2064)) ptr
{-# LINE 132 "FlyCapBase.hsc" #-}
             fV <- ((\hsc_ptr -> peekByteOff hsc_ptr 2576)) ptr
{-# LINE 133 "FlyCapBase.hsc" #-}
             fBT <- ((\hsc_ptr -> peekByteOff hsc_ptr 3088)) ptr
{-# LINE 134 "FlyCapBase.hsc" #-}
             mBS <- ((\hsc_ptr -> peekByteOff hsc_ptr 3600)) ptr
{-# LINE 135 "FlyCapBase.hsc" #-}
             bTF <- ((\hsc_ptr -> peekByteOff hsc_ptr 3608)) ptr
{-# LINE 136 "FlyCapBase.hsc" #-}
             iV <-  ((\hsc_ptr -> peekByteOff hsc_ptr 3616)) ptr
{-# LINE 137 "FlyCapBase.hsc" #-}
             cR <-  ((\hsc_ptr -> peekByteOff hsc_ptr 3620)) ptr
{-# LINE 138 "FlyCapBase.hsc" #-}
             mjV <- ((\hsc_ptr -> peekByteOff hsc_ptr 4236)) ptr 
{-# LINE 139 "FlyCapBase.hsc" #-}
             mnV <- ((\hsc_ptr -> peekByteOff hsc_ptr 4240)) ptr
{-# LINE 140 "FlyCapBase.hsc" #-}
             uDN <- ((\hsc_ptr -> peekByteOff hsc_ptr 4244)) ptr
{-# LINE 141 "FlyCapBase.hsc" #-}
             u1 <- ((\hsc_ptr -> peekByteOff hsc_ptr 4756)) ptr
{-# LINE 142 "FlyCapBase.hsc" #-}
             u2 <- ((\hsc_ptr -> peekByteOff hsc_ptr 5268)) ptr
{-# LINE 143 "FlyCapBase.hsc" #-}
             mA <- ((\hsc_ptr -> peekByteOff hsc_ptr 5780)) ptr
{-# LINE 144 "FlyCapBase.hsc" #-}
             iA <- ((\hsc_ptr -> peekByteOff hsc_ptr 5786)) ptr
{-# LINE 145 "FlyCapBase.hsc" #-}
             sM <- ((\hsc_ptr -> peekByteOff hsc_ptr 5790)) ptr
{-# LINE 146 "FlyCapBase.hsc" #-}
             dG <- ((\hsc_ptr -> peekByteOff hsc_ptr 5794)) ptr
{-# LINE 147 "FlyCapBase.hsc" #-}
             r <- ((\hsc_ptr -> peekByteOff hsc_ptr 5812)) ptr
{-# LINE 148 "FlyCapBase.hsc" #-}
             return CamInfo { serialNum = sN, iFType = iT, colorCam = cC, modelName = modN, vendorName = vN, sensorInfo = sI,sensorRes = sR, driverName = dN , firmwareVersion = fV, firmwareBuildTime = fBT, maxBusSpeed = mBS, bayerTileFormat = bTF, iidcVer = iV, configRom = cR, majorVersion = mjV, minorVersion = mnV , userDefName = uDN, xmlURL1 = u1, xmlURL2 = u2, macAddress = mA, ipAddress = iA, subnetMask = sM , defaultGateway = dG, reserved = r}
  poke ptr (CamInfo sN iT cC modN vN sI sR dN fV fBT mBS bTF iV cR mjV mnV uDN u1 u2 mA iA sM dG r) = do 
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr sN
{-# LINE 151 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr iT
{-# LINE 152 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr cC
{-# LINE 153 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr modN
{-# LINE 154 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 528)) ptr vN
{-# LINE 155 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1040)) ptr sI
{-# LINE 156 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1552)) ptr sR
{-# LINE 157 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2064)) ptr dN
{-# LINE 158 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2576)) ptr fV
{-# LINE 159 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 3088)) ptr fBT
{-# LINE 160 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 3600)) ptr mBS
{-# LINE 161 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 3608)) ptr bTF
{-# LINE 162 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 3616)) ptr iV
{-# LINE 163 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 3620)) ptr cR
{-# LINE 164 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4236)) ptr mjV 
{-# LINE 165 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4240)) ptr mnV
{-# LINE 166 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4244)) ptr uDN
{-# LINE 167 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4756)) ptr u1
{-# LINE 168 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 5268)) ptr u2
{-# LINE 169 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 5780)) ptr mA
{-# LINE 170 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 5786)) ptr iA
{-# LINE 171 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 5790)) ptr sM
{-# LINE 172 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 5794)) ptr dG
{-# LINE 173 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 5812)) ptr r
{-# LINE 174 "FlyCapBase.hsc" #-}


data CImage = CImage { rows        :: CUInt
                     , cols        :: CUInt  
                     , stride      :: CUInt  
                     , pData       :: Ptr CUChar  
                     , dataSize    :: CUInt  
                     , format      :: CInt -- an fc2PixelFormat enum
                     , bayerFormat :: CInt -- an enum
                     , imageIMPl   :: Ptr ()  
                     } deriving (Show)
              
instance Storable CImage where
  sizeOf _ = ((48))
{-# LINE 188 "FlyCapBase.hsc" #-}
  alignment _ = alignment (undefined :: CDouble) 
  peek ptr = do
    r <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 191 "FlyCapBase.hsc" #-}
    c <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 192 "FlyCapBase.hsc" #-}
    s <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 193 "FlyCapBase.hsc" #-}
    p <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 194 "FlyCapBase.hsc" #-}
    dS <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 195 "FlyCapBase.hsc" #-}
    f <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 196 "FlyCapBase.hsc" #-}
    bF <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 197 "FlyCapBase.hsc" #-}
    iI <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 198 "FlyCapBase.hsc" #-}
    return CImage {rows = r, cols = c, stride = s, pData = p, dataSize = dS, format = f, bayerFormat = bF, imageIMPl = iI}
  poke ptr (CImage r c s p dS f bF iI) = do 
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr r
{-# LINE 201 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr c
{-# LINE 202 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr s
{-# LINE 203 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr p
{-# LINE 204 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr dS
{-# LINE 205 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr f
{-# LINE 206 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr bF
{-# LINE 207 "FlyCapBase.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr iI
{-# LINE 208 "FlyCapBase.hsc" #-}

highToC :: (Eq a, Ord a, Eq b, Ord b) => [(a,b)] -> b -> a -> b
highToC constMap valDefault query = 
  maybe valDefault id (lookup query constMap)

cToHigh :: (Eq a, Ord a, Eq b, Ord b) => [(a,b)] -> a -> b -> a
cToHigh constMap valDefault query =
  maybe valDefault id (lookup query (fl constMap))
  where fl m = map (\(f,s) -> (s,f)) m
        
data VideoMode = VM160x120 | VM320x240 | VM640x480_YUV411 | VM640x480_YUV422 | VM640x480_RGB | VM640x480_Y8 | VM640x480_Y16 | VM800x600_YUV422 | VM800x600_RGB | VM800x600_Y8 | VM800x600_Y16 | VM1024x768_YUV422 | VM1024x768_RGB | VM1024x768_Y8 | VM1024x768_Y16 | VM1280x960_YUV422 | VM1280x960_RGB | VM1280x960_Y8 | VM1280x960_Y16 | VM1600x1200_YUV422 | VM1600x1200_RGB | VM1600x1200_Y8 | VM1600x1200_Y16 | VMFormat7 | VMNum | VMForce32b deriving (Ord,Eq,Show)

vmMap :: [(VideoMode, CInt)]
vmMap = [ ( VM800x600_Y8, 9)
{-# LINE 222 "FlyCapBase.hsc" #-}
        , ( VM160x120, 0) 	
{-# LINE 223 "FlyCapBase.hsc" #-}
        , ( VM320x240, 1)
{-# LINE 224 "FlyCapBase.hsc" #-}
        , ( VM640x480_YUV411, 2)
{-# LINE 225 "FlyCapBase.hsc" #-}
        , ( VM640x480_YUV422, 3)
{-# LINE 226 "FlyCapBase.hsc" #-}
        , ( VM640x480_RGB, 4)
{-# LINE 227 "FlyCapBase.hsc" #-}
        , ( VM640x480_Y8, 5)
{-# LINE 228 "FlyCapBase.hsc" #-}
        , ( VM640x480_Y16, 6)
{-# LINE 229 "FlyCapBase.hsc" #-}
        , ( VM800x600_YUV422, 7)
{-# LINE 230 "FlyCapBase.hsc" #-}
        , ( VM800x600_RGB, 8)
{-# LINE 231 "FlyCapBase.hsc" #-}
        , ( VM800x600_Y16, 10)
{-# LINE 232 "FlyCapBase.hsc" #-}
        , ( VM1024x768_YUV422, 11)
{-# LINE 233 "FlyCapBase.hsc" #-}
        , ( VM1024x768_RGB, 12) 
{-# LINE 234 "FlyCapBase.hsc" #-}
        , ( VM1024x768_Y8, 13)
{-# LINE 235 "FlyCapBase.hsc" #-}
        , ( VM1024x768_Y16, 14)
{-# LINE 236 "FlyCapBase.hsc" #-}
        , ( VM1280x960_YUV422, 15)
{-# LINE 237 "FlyCapBase.hsc" #-}
        , ( VM1280x960_RGB, 16)
{-# LINE 238 "FlyCapBase.hsc" #-}
        , ( VM1280x960_Y8, 17)
{-# LINE 239 "FlyCapBase.hsc" #-}
        , ( VM1280x960_Y16, 18) 
{-# LINE 240 "FlyCapBase.hsc" #-}
        , (VM1600x1200_YUV422, 19)
{-# LINE 241 "FlyCapBase.hsc" #-}
        , (VM1600x1200_RGB, 20)
{-# LINE 242 "FlyCapBase.hsc" #-}
        , (VM1600x1200_Y8, 21)
{-# LINE 243 "FlyCapBase.hsc" #-}
        , (VM1600x1200_Y16, 22)
{-# LINE 244 "FlyCapBase.hsc" #-}
        , (VMFormat7, 23)
{-# LINE 245 "FlyCapBase.hsc" #-}
        , (VMNum, 24)
{-# LINE 246 "FlyCapBase.hsc" #-}
        , (VMForce32b, 2147483647)]
{-# LINE 247 "FlyCapBase.hsc" #-}

vmFromC :: CInt -> VideoMode
vmFromC = cToHigh vmMap VM640x480_Y8

vmToC :: VideoMode -> CInt
vmToC = highToC vmMap (snd (head vmMap))

data FrameRate = Fr1_875 |Fr3_75 | Fr7_5 | Fr_15 | Fr_30 | Fr_60 | Fr_120 | Fr_240 | FrFormat7 | FrNumFR | FrForce32b 
               deriving (Eq, Ord, Show)
  
frMap :: [(FrameRate, CInt)]  
frMap = [ (Fr_30, 4)
{-# LINE 259 "FlyCapBase.hsc" #-}
        , (Fr1_875, 0)
{-# LINE 260 "FlyCapBase.hsc" #-}
        , (Fr3_75, 1)
{-# LINE 261 "FlyCapBase.hsc" #-}
        , (Fr7_5, 2)
{-# LINE 262 "FlyCapBase.hsc" #-}
        , (Fr_15, 3)
{-# LINE 263 "FlyCapBase.hsc" #-}
        , (Fr_60, 5)
{-# LINE 264 "FlyCapBase.hsc" #-}
        , (Fr_120, 6)
{-# LINE 265 "FlyCapBase.hsc" #-}
        , (Fr_240, 7)
{-# LINE 266 "FlyCapBase.hsc" #-}
        , (FrFormat7, 8)
{-# LINE 267 "FlyCapBase.hsc" #-}
        , (FrNumFR, 9)
{-# LINE 268 "FlyCapBase.hsc" #-}
        , (FrForce32b, 2147483647) ]
{-# LINE 269 "FlyCapBase.hsc" #-}



frFromC :: CInt -> FrameRate
frFromC = cToHigh frMap Fr_30 
  
frToC :: FrameRate -> CInt
frToC =  highToC frMap (snd (head frMap))

--functions used in tracker.c:  

foreign import ccall unsafe "FlyCapture2_C.h fc2GetNumOfCameras"
   fc2getNumOfCameras :: Context -> Ptr CUInt -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2CreateContext"
   fc2CreateContext :: Ptr (Context) -> IO Error
                      
foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraFromIndex"
   fc2GetCameraFromIndex :: Context -> CUInt -> Ptr PGRGuid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraFromSerialNumber"
   fc2GetCameraFromSerialNumber :: Context -> CUInt -> Ptr PGRGuid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2Connect"
  fc2Connect :: Context -> Ptr PGRGuid -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetLibraryVersion"
  fc2GetLibraryVersion :: Ptr fc2Version -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetCameraInfo"
  fc2GetCameraInfo :: Context -> Ptr CamInfo -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2SetVideoModeAndFrameRate"
  fc2SetVideoModeAndFrameRate :: Context -> CInt -> CInt -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2StartSyncCapture"
  fc2StartSyncCapture :: CUInt -> Ptr Context -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2StartCapture"
  fc2StartCapture :: Context -> IO Error
 
foreign import ccall unsafe "FlyCapture2_C.h fc2RetrieveBuffer"
  fc2RetrieveBuffer :: Context ->Ptr CImage -> IO Error
                         
foreign import ccall unsafe "FlyCapture2_C.h fc2CreateImage" 
 fc2CreateImage :: Ptr CImage -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2StopCapture"
  fc2StopCapture :: Context -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2GetImageData"
  fc2GetImageData :: Ptr CImage -> Ptr ImageData -> IO Error
 
foreign import ccall unsafe "FlyCapture2_C.h fc2Disconnect"
  fc2Disconnect :: Context -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2DestroyContext"
  fc2DestroyContext :: Context -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2DestroyImage"
  fc2DestroyImage :: Ptr CImage -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2AVIAppend"
  fc2AVIAppend :: AVIContext -> Ptr CImage -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2AVIClose"
  fc2AVIClose :: AVIContext -> IO Error
                 
foreign import ccall unsafe "FlyCapture2_C.h fc2AVIOpen"
  fc2AVIOpen :: AVIContext -> Ptr CChar -> Ptr AVIOption -> IO Error

foreign import ccall unsafe "FlyCapture2_C.h fc2CreateAVI"
  fc2CreateAVI :: Ptr AVIContext -> IO Error
                  
foreign import ccall unsafe "FlyCapture2_C.h fc2DestroyAVI"
  fc2DestroyAVI :: AVIContext -> IO Error

ctoJImage :: CImage -> IO B.ByteString -- IO DynamicImage
ctoJImage (CImage r c _ p _ _ _ _ ) = do
  let h = fromIntegral r
  let w = fromIntegral c
  print $ "h was: " ++ show h
  print $ "w was: " ++ show w
  a <- peekArray (h*w) p -- arr: [CUChar]
  print $ "array was" ++ show a
  let arr = map (fromIntegral) a 
  let bs = B.pack arr
  return bs
 
