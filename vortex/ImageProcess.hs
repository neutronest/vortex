module ImageProcess where

import Codec.Picture
import Data.Vector.Storable (Vector, length, toList)
import Numeric.LinearAlgebra
--import Data.ByteString



imagePath :: FilePath
imagePath = "./test.jpg"


getDynamicImage :: FilePath -> IO(Maybe DynamicImage)
getDynamicImage filepath = do
  res <- readImage filepath
  case res of
    Left err -> print err >> return Nothing
    Right image -> return (Just image)

readImageInfo :: DynamicImage -> Image PixelRGB8
readImageInfo image =
  let imagePixel8@(Image w h rgbs) = convertRGB8 image in
  imagePixel8


getImageInfo :: FilePath -> IO(Maybe (Image PixelRGB8))
getImageInfo filepath = do
  res <- readImage filepath
  case res of
    Left err -> print err >> return Nothing
    Right image -> return $ Just (readImageInfo image)
  
getImageWidth :: Image PixelRGB8 -> Int
getImageWidth image =
  let (Image w _ _) = image in
  w

getImageHeight :: Image PixelRGB8 -> Int
getImageHeight image =
  let (Image _ h _) = image in
  h

getImageRGBs :: Image PixelRGB8 -> Data.Vector.Storable.Vector(PixelBaseComponent Pixel8)
getImageRGBs image =
  let (Image _ _ rgbs) = image in
  rgbs


printPixels :: Show a => [a] -> IO()
printPixels [] = return()
printPixels (xr:xg:xb:xs) =
  print [xr, xg, xb] >>
  printPixels xs


genRGBList :: (Show a, Integral a, Fractional b) => [a] -> ([b], [b], [b])
genRGBList [] = ([], [], [])
genRGBList l =
  let func [] rlist glist blist = ((reverse rlist), (reverse glist), (reverse blist))
      func (xr:xg:xb:xs) rlist glist blist = func xs (((fromIntegral xr)/256.0):rlist) (((fromIntegral xg)/256.0):glist) (((fromIntegral xb)/256.0):blist) in
  func l [] [] []

genMatrixFromList :: [R] -> Int -> Int -> Maybe (Matrix R)
genMatrixFromList [] _ _ = Nothing
genMatrixFromList l w h = Just ((w >< h) l)


testImageProcess :: FilePath -> IO()
testImageProcess filepath = do
    imageM <- getImageInfo filepath
    case imageM of
      Nothing -> putStrLn "nothing.."
      Just imagePixel8 ->
        putStrLn "the width of image: " >>
        print (getImageWidth imagePixel8) >>
        putStrLn "the height ofimage: " >>
        print (getImageHeight imagePixel8) >>
        print (Data.Vector.Storable.length (getImageRGBs imagePixel8)) >>
        let rgbs = getImageRGBs imagePixel8 in
        let w = getImageWidth imagePixel8 in
        let h = getImageHeight imagePixel8 in
        let rgbsList = Data.Vector.Storable.toList rgbs in
        let (rlist, glist, blist) = genRGBList rgbsList in
        print (Prelude.length rlist) >>
        print (Prelude.length glist) >>
        print (Prelude.length blist) >>
        let Just rMat = genMatrixFromList rlist w h in
        let Just gMat = genMatrixFromList glist w h in
        let Just bMat = genMatrixFromList blist w h in
        print rMat







{-


decodeImageWidth :: IO(Maybe (Image PixelRGB8)) -> IO(Maybe Int))
decodeImageWidth imageM = do
  maybeImage <- imageM
  case maybeImage of
    Nothing -> return Nothing
    Just (Image w h rgbs) -> return (Just w)
  

decodeImageHeight :: IO(Maybe(Image PixelRGB8)) -> IO(Maybe Int)
decodeImageHeight imageM = do
  maybeImage <- imageM
  case maybeImage of
    Nothing -> return Nothing
    Just (Image w h rgbs) -> return (Just h)

decodeImageRGBs :: IO(Maybe (Image PixelRGB8)) -> IO(Maybe (Vector(PixelBaseComponent Pixel8)))
decodeImageRGBs imageM = do
  maybeImage <- imageM
  case maybeImage of
    Nothing -> return Nothing
    Just (Image w h rgbs) -> return (Just rgbs)


readRGBsPixel8 :: Maybe DynamicImage -> IO(Maybe (Vector(PixelBaseComponent Pixel8)))
readRGBsPixel8 Nothing = return Nothing
readRGBsPixel8 (Just image) =
  let imagePixel8@(Image _ _ rgbs) = convertRGB8 image in return (Just rgbs)

getRGBs :: FilePath -> IO(Maybe (Vector(PixelBaseComponent Pixel8)))
getRGBs filepath = do
  dynamicImage <- getDynamicImage filepath
  rgbs <- readRGBsPixel8 dynamicImage
  return rgbs

-}
{-}
readRGBsPixel8 :: DynamicImage -> Maybe (Vector (PixelBaseComponent Pixel8))
readRGBsPixel8 (ImageY8 image@(Image _ _ rgbs)) = Just rgbs
readRGBsPixel8 _ = Nothing

readRGBsPixel16 :: DynamicImage -> Maybe (Vector (PixelBaseComponent Pixel16))
readRGBsPixel16 :: (ImageY16 image@(Image _ _ rgbs)) = Just rgbs
readRGBsPixel16 _ = Nothing

readRGBsPixelYF :: DynamicImage -> Maybe (Vector (PixelBaseComponent PixelF))
readRGBsPixelYF :: (ImageF image@(Image _ _ rgbs)) = Just rgbs
readRGBsPixelYF _ = Nothing

readRGBsPixelYA8 :: DynamicImage -> Maybe (Vector (PixelBaseComponent PixelYA8))
readRGBsPixelYA8 :: (ImageYA8 image@(Image _ _ rgbs)) = Just rgbs
readRGBsPixelYA8 _ = Nothing

readRGBsPixelYA16 :: DynamicImage -> Maybe (Vector (PixelBaseComponent PixelYA16))
readRGBsPixelYA16 :: (ImageYA16 image@(Image _ _ rgbs)) = Just rgbs
readRGBsPixelYA16 _ = Nothing



getRGBsImage :: forall a. Pixel a => DynamicImage -> Vector (PixelBaseComponent a)
getRGBsImage (ImageY8 image@(Image _ _ rgbs@(MVector (Word8 _)))) = rgbs
getRGBsImage (ImageY16 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageYF image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageYA8 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageRGB8 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageRGB16 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageRGBF image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageRGBA8 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageRGBA16 image@(Image _ _ rgbs)) = rgbs 
getRGBsImage (ImageYCbCr8 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageCMYK8 image@(Image _ _ rgbs)) = rgbs
getRGBsImage (ImageCMYK16 image@(Image _ _ rgbs)) = rgbs

readRGBs :: forall a. Pixel a => FilePath  -> IO(Maybe (Vector (PixelBaseComponent a)))
readRGBs filepath = do
  res <- getDynamicImage filepath
  case res of
    Nothing -> print "we got nothing!" >> return Nothing
    Just image -> return (Just (getRGBsImage image))

-}
{-
getRGBs :: Image -> Vector(PixelBaseComponent Word8)
getRGBs (Image _ _ rgbs) = rgbs 
-}

justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x
