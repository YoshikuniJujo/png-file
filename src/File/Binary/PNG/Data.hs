{-# LANGUAGE TypeFamilies #-}

module File.Binary.PNG.Data (
	PNG(..), PNGImage(..), PNGError(..)
) where

import Data.ByteString.Lazy

data PNG pi = PNG PNGHeader PNGValues [(String, ByteString)] pi

data PNGHeader = PNGHeader {
 }

data PNGValues = PNGValues {
 }

class PNGImage pi where
	type PNGColor pi
	type PNGError pi

	goNext :: pi -> Either (PNGError pi) pi
	getXY :: pi -> (Int, Int)

	goUp :: pi -> Either (PNGError pi) pi
	goDown :: pi -> Either (PNGError pi) pi
	goLeft :: pi -> Either (PNGError pi) pi
	goRight :: pi -> Either (PNGError pi) pi

	getPixel :: pi -> PNGColor pi
	setPixel :: pi -> PNGColor pi -> pi

	toPalet :: pi -> Either (PNGError pi) pi
	toGrey :: pi -> Either (PNGError pi) pi
	fromAlpha :: pi -> Either (PNGError pi) pi
	toInterlace :: pi -> pi
	fromInterlace :: pi -> pi
