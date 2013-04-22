{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module File.Binary.PNG.Data (
	PNG(..), PNGImage(..), PNGColorType(..), PNGHeader(..),
	PNGImageL(..)
) where

import qualified Data.ByteString.Lazy as BSL

data PNG pi = PNG PNGValues [(String, BSL.ByteString)] pi

data PNGValues = PNGValues {
 }

data PNGHeader = PNGHeader {
	pngWidth :: Int,
	pngHeight :: Int,
	pngDepth :: Int,
	pngColorType :: PNGColorType,
	pngCompType :: Int,
	pngFilterType :: Int,
	pngInterlaceType :: Int
 } deriving Show

data PNGColorType
	= PNGTypeIndex { piTrans :: [Int] }
	| PNGTypeGrey { pgTrans :: Maybe Int } | PNGTypeGreyAlpha
	| PNGTypeColor { pcTrans :: Maybe (Int, Int, Int) } | PNGTypeColorAlpha
	deriving Show

class PNGColor (PNGImageColor pi) => PNGImage pi where
	type PNGImageColor pi
	type PNGImageError pi

	makePNGImage :: PNGHeader -> BSL.ByteString -> pi
	fromPNGImage :: pi -> (PNGHeader, BSL.ByteString)

	goNext :: pi -> Either (PNGImageError pi) pi
	getXY :: pi -> (Int, Int)

	goUp :: pi -> Either (PNGImageError pi) pi
	goDown :: pi -> Either (PNGImageError pi) pi
	goLeft :: pi -> Either (PNGImageError pi) pi
	goRight :: pi -> Either (PNGImageError pi) pi

	getPixel :: pi -> PNGImageColor pi
	setPixel :: pi -> PNGImageColor pi -> pi

--	toPalet :: pi -> Either (PNGImageError pi) pi
--	toGrey :: pi -> Either (PNGImageError pi) pi
--	fromAlpha :: pi -> Either (PNGImageError pi) pi
	toInterlace :: pi -> pi
	fromInterlace :: pi -> pi

class PNGColor pc where

data PNGImageL = PNGImageL {
	pilInterlace :: Bool,
	pilBackLines :: [([PNGImageLColor], [PNGImageLColor])],
	pilForwardLines :: [([PNGImageLColor], [PNGImageLColor])]}
	deriving Show

data PNGImageLColor = PNGImageLColor Int Int Int Int deriving Show

instance PNGImage PNGImageL where
	type PNGImageColor PNGImageL = PNGImageLColor
	makePNGImage = makePNGImageL

instance PNGColor PNGImageLColor where

makePNGImageL :: PNGHeader -> BSL.ByteString -> PNGImageL
makePNGImageL PNGHeader {
	pngWidth = w,
	pngHeight = h,
	pngDepth = 8,
	pngColorType = PNGTypeColor Nothing,
	pngCompType = 0,
	pngFilterType = 0,
	pngInterlaceType = 0
 } bs = PNGImageL False [] []
