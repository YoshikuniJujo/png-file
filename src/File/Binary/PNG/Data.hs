{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, TupleSections #-}

module File.Binary.PNG.Data (
	PNG(..), PNGImage(..), PNGColorType(..), PNGHeader(..),
	PNGImageL(..), PNGImageLColor(..)
) where

import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Int
import Data.Maybe

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
 } bs = PNGImageL False [] $ map ([] ,) $ bsToImageA w bs

bsToImageA :: Int -> BSL.ByteString -> [[PNGImageLColor]]
bsToImageA w = map (map convert) . bsToImage w
	where
	convert (r, g, b) = PNGImageLColor
		(fromIntegral r) (fromIntegral g) (fromIntegral b) 255

setpre :: Int -> BSL.ByteString -> BSL.ByteString -> BSL.ByteString
setpre w pre rgb
	| fromIntegral (BSL.length pre) == w * 3 + 3 = BSL.drop 3 pre `BSL.append` rgb
	| otherwise = error "bad pre" -- pre `append` rgb

bsToImage w bs = reverse $ map reverse $
	bsToImage' w (BSL.replicate (fromIntegral w * 3 + 3) 0) bs []

bsToImage' :: Int -> BSL.ByteString -> BSL.ByteString -> [[(Word8, Word8, Word8)]] ->
	[[(Word8, Word8, Word8)]]
bsToImage' w pre bs rets
	| BSL.null bs = rets
	| otherwise = let
		Just (filter, dat') = BSL.uncons bs
		(pre', ret, dat'') = bsToLine filter w 0 pre Nothing [] dat' in
		bsToImage' w pre' dat'' (ret : rets)

bsToLine :: Word8 -> Int -> Int -> BSL.ByteString ->
	Maybe (Word8, Word8, Word8) ->
	[(Word8, Word8, Word8)] -> BSL.ByteString ->
	(BSL.ByteString, [(Word8, Word8, Word8)], BSL.ByteString)
bsToLine filter w x pre left ret dat
	| x < w = let
		tToL (r, g, b) = [r, g, b]
		lToT [r, g, b] = (r, g, b)
		l = fromMaybe (0, 0, 0) left
		lu = (\[r, g, b] -> (r, g, b)) $
			maybe [0, 0, 0] (const $ take' 3 pre) left
		color'@[r, g, b] = getColor filter
			(tToL l) (take' 3 $ BSL.drop 3 pre) (tToL lu) (take' 3 dat)
		color = (r, g, b) in
		bsToLine filter w (x + 1) (setpre w pre $ BSL.pack color')
			(Just color) (color : ret) $ BSL.drop 3 dat
	| otherwise = (pre, ret, dat)

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 _ [] [] [] [] = []
zipWith4 f (x : xs) (y : ys) (z : zs) (w : ws) = f x y z w : zipWith4 f xs ys zs ws

getColor 0 left up leftup rgb = rgb
getColor 1 left up leftup rgb = zipWith (+) left rgb
getColor 2 left up leftup rgb = zipWith (+) up rgb
getColor 4 left up leftup rgb = zipWith4 getByte4'' left up leftup rgb

getByte4'' :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
getByte4'' left up leftup rgb = paeth' left up leftup + rgb

paeth' :: Word8 -> Word8 -> Word8 -> Word8
paeth' a b c = let
	[a', b', c'] = map fromIntegral [a, b, c]
	p :: Int = a' + b' - c'
	pa = abs $ p - a'
	pb = abs $ p - b'
	pc = abs $ p - c' in
	if pa <= pb && pa <= pc then a else
		if pb <= pc then b else c

take' :: Int64 -> BSL.ByteString -> [Word8]
take' n = BSL.unpack . BSL.take n
