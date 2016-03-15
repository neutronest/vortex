module ImageProcess where

import Control.Monad
import Data.Functor
import Codec.Picture
import Data.Vector.Storable (Vector)
import Control.Monad.Trans.Maybe
--import Data.ByteString



imagePath :: FilePath
imagePath = "./test.jpg"


getDynamicImage :: FilePath -> IO(Maybe DynamicImage)
getDynamicImage filepath = do
  res <- readImage filepath
  case res of
    Left err -> print err >> return Nothing
    Right image -> return (Just image)


readRGBsPixel8 :: Maybe DynamicImage -> IO(Maybe (Vector(PixelBaseComponent Pixel8)))
readRGBsPixel8 Nothing = return Nothing
readRGBsPixel8 (Just image) =
  let imagePixel8@(Image _ _ rgbs) = convertRGB8 image in return (Just rgbs)

getRGBs :: FilePath -> IO(Maybe (Vector(PixelBaseComponent Pixel8)))
getRGBs filepath = do
  dynamicImage <- getDynamicImage filepath
  rgbs <- readRGBsPixel8 dynamicImage
  return rgbs

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
