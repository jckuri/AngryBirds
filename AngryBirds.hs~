-- AngryBirds.hs (Author: Juan Carlos Kuri Pinto)

module Main(main) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified Codec.Picture as CP
import qualified Data.Vector.Storable as DV
import Data.IORef

-- FUNCTIONS FOR READING IMAGES

cuCharToPtr :: [CUChar] -> IO (Ptr CUChar)
cuCharToPtr list = newArray list
 
vectorToList vector = DV.toList vector

vectorToList2 each inserts vector = 
 foldr accumulator [] [0..(n-1)]
 where 
  n = (DV.length vector) `div` 3
  accumulator index acc =
   r:g:b:a:acc
   where
    r = vector DV.! (index*3+0)
    g = vector DV.! (index*3+1)
    b = vector DV.! (index*3+2)
    a = 255
 
 
word8ToCUChar word8 = fromIntegral word8 :: CUChar

word8ListToCUCharList word8List =
 map (word8ToCUChar . (`div` 2)) word8List
 
data MemoryImage = MemoryImage {
 width :: Int,
 height :: Int,
 imagePtr :: Ptr CUChar
}

transformImageRGB8 image =
 let
  iWidth = CP.imageWidth image
  iHeight = CP.imageHeight image
  imageVector = CP.imageData image
  imageList = vectorToList2 iWidth (iWidth `mod` 4) imageVector
  -- imageList = vectorToList imageVector
  iList = word8ListToCUCharList imageList
 in
  cuCharToPtr iList >>= \iPtr ->
  putStrLn ("ImageRGB8 Width: " ++ show iWidth ++ " Height: " ++ show iHeight ++ " Size: " ++ show (length iList)) >>
  putStrLn ("componentCount of pixelAt(0,0): " ++ show (CP.componentCount (CP.pixelAt image 0 0))) >>
  return MemoryImage {width=iWidth,height=iHeight,imagePtr=iPtr}

rgba8VectorToRgbList2 vector =
 foldr accumulator [] [0..(n-1)]
 where 
  n = (DV.length vector) `div` 4
  accumulator index acc =
   r:g:b:acc
   where
    r = vector DV.! (index*4+0)
    g = vector DV.! (index*4+1)
    b = vector DV.! (index*4+2)
    
rgba8VectorToRgbaList3 vector =
 foldr accumulator [] [0..(n-1)]
 where 
  n = (DV.length vector) `div` 4
  accumulator index acc =
   r:g:b:a:acc
   where
    r = vector DV.! (index*4+0)
    g = vector DV.! (index*4+1)
    b = vector DV.! (index*4+2)
    a = vector DV.! (index*4+3)
    
rgba8VectorToRgbaList vector = DV.toList vector
    
    
{-
vectorToList2 each inserts vector = 
 foldr accumulator [] [0..(n-1)]
 where 
  n = (DV.length vector) `div` 3
  accumulator index acc =
   if index `mod` each == each - 1 then
    r:g:b:(replicate inserts 0) ++ acc
   else
    r:g:b:acc
   where
    r = vector DV.! (index*3+0)
    g = vector DV.! (index*3+1)
    b = vector DV.! (index*3+2)
-}
  
transformImageRGBA8 image =
 let
  iWidth = CP.imageWidth image
  iHeight = CP.imageHeight image
  imageVector = CP.imageData image
  imageList = rgba8VectorToRgbaList imageVector
  iList = word8ListToCUCharList imageList
 in
  cuCharToPtr iList >>= \iPtr ->
  putStrLn ("ImageRGBA8 Width: " ++ show iWidth ++ " Height: " ++ show iHeight ++ " Size: " ++ show (length iList)) >>
  return MemoryImage {width=iWidth,height=iHeight,imagePtr=iPtr}

getImageFormat img =
 case img of
  CP.ImageY8 i -> "ImageY8"
  CP.ImageYF i -> "ImageYF"
  CP.ImageYA8 i -> "ImageYA8"
  CP.ImageRGB8 i -> "ImageRGB8"
  CP.ImageRGBF i -> "ImageRGBF"
  CP.ImageRGBA8 i -> "ImageRGBA8"
  CP.ImageYCbCr8 i -> "ImageYCbCr8"
  
processImage :: CP.DynamicImage -> IO (Either String MemoryImage)
processImage img =
 case img of
  CP.ImageRGB8 image ->
   transformImageRGB8 image >>= \memoryImage ->
   return (Right memoryImage)
  CP.ImageRGBA8 image ->
   transformImageRGBA8 image >>= \memoryImage ->
   return (Right memoryImage)
  otherwise -> 
   return (Left ("Image format not supported: " ++ getImageFormat img))
  
readImageFile :: String -> IO (Either String MemoryImage)
readImageFile filename =
 CP.readImage filename >>= \image ->
 -- CP.readGif filename >>= \image ->
 case image of 
  (Left s) -> return (Left ("Error: " ++ s))
  (Right d) -> processImage d

--main = readImageFile "../cell_untouched.gif"

-- FUNCTIONS FOR OPENGL DRAWINGS

getSize memoryImage =
 Size (fromIntegral w :: GLsizei) (fromIntegral h :: GLsizei)
 where
  w = width memoryImage
  h = height memoryImage

-- glEnable(GL_BLEND); and glBlendFunc(GL_SRC_ALPHA,GL_ONE)  

drawImagePixels :: GLfloat -> GLfloat -> Either String MemoryImage -> IO ()
drawImagePixels x y eImage =
 case eImage of
  Left error -> 
   putStrLn error
  Right memoryImage -> 
   rasterPos (Vertex2 x y) >>
   drawPixels (getSize memoryImage) (PixelData RGBA Byte (imagePtr memoryImage))

path = "../images/"
imageFile1 = path ++ "cell_untouched.gif"
imageFile2 = path ++ "cell_touched.gif"
imageFile3 = path ++ "smiley_smile.gif"
imageFile4 = path ++ "icon.gif"
imageFile5 = "../mine-equations-2.png"
imageFile6 = "../mine-equations-3.png"
imageFile7 = "background.gif"

imageFiles3 = ["background.gif","angry-bird.gif","explosion.gif","glass.gif","pig.gif","rock.gif","wood.gif"]
imageFiles2 = ["background.gif","explosion.gif"]

imageFiles = map ("png/" ++) ["background.png","angry-bird.png","explosion.png","glass.png","pig.png","rock.png","wood.png"]
--imageFiles = map ("gif/" ++) ["background.gif","angry-bird.gif","explosion.gif","glass.gif","pig.gif","rock.gif","wood.gif"]

--images = newIORef ([]:[MemoryImage])

readImages = mapM readImageFile imageFiles

drawImages images = 
 mapM_ drawImageIndex [0..(fromIntegral (length images) :: GLfloat) - 1] 
 where
  drawImageIndex i = drawImagePixels (-1+i*0.1) (1) (images !! (floor i :: Int))
  
display images = 
 clear [ColorBuffer,DepthBuffer] >>
 loadIdentity >>
 color (Color3 (1.0::GLfloat) 1.0 1.0) >>
 rasterPos (Vertex2 (-1::GLfloat) (1::GLfloat)) >>
 pixelZoom $= (1::GLfloat,-1::GLfloat) >>
 blend $= Enabled >>
 blendFunc $= (SrcAlpha, OneMinusSrcAlpha) >>
 drawImages images >>
 swapBuffers

reshape s@(Size w h) =
 viewport $= (Position 0 0, s)

size = Size 1028 640 
 
main =
 getArgsAndInitialize >>= \(programName,_) ->
 readImages >>= \imagesRead ->
 --newIORef () >>= \images -> 
 initialDisplayMode $= [WithDepthBuffer,DoubleBuffered] >>
 createWindow "Angry Birds" >>
 windowSize $= size >>
 reshapeCallback $= Just reshape >>
 displayCallback $= (display imagesRead) >>
 mainLoop
