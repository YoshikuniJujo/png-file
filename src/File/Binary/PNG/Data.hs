{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, TupleSections #-}

module File.Binary.PNG.Data (
	PNG(..), PNGImage(..), PNGColorType(..), PNGHeader(..),
	PNGImageL(..), PNGImageLColor(..)
) where

import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Int
import Data.Maybe
import Data.Bits

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
	pngInterlaceType :: Int,
	pngPalette :: [(Int, Int, Int)]
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
	pilBits :: Int,
	pilBackLines :: [([PNGImageLColor], [PNGImageLColor])],
	pilForwardLines :: [([PNGImageLColor], [PNGImageLColor])]}
	deriving Show

data PNGImageLColor = PNGImageLColor Int Int Int Int deriving Show

instance PNGImage PNGImageL where
	type PNGImageColor PNGImageL = PNGImageLColor
	type PNGImageError PNGImageL = String
	makePNGImage = makePNGImageL
	fromPNGImage = fromPNGImageL

	goUp = pngImageLUp
	goDown = pngImageLDown
	goLeft = pngImageLLeft
	goRight = pngImageLRight

instance PNGColor PNGImageLColor where

pngImageLUp, pngImageLDown, pngImageLLeft, pngImageLRight ::
	PNGImageL -> Either String PNGImageL
pngImageLUp (PNGImageL _ _ [] _) = fail "can't go up"
pngImageLUp (PNGImageL False bits (u : us) ds) =
	return $ PNGImageL False bits us (u : ds)
pngImageLDown (PNGImageL _ _ _ []) = fail "can't go down"
pngImageLDown (PNGImageL False bits us (d : ds)) =
	return $ PNGImageL False bits (d : us) ds
pngImageLLeft (PNGImageL False bits us ds) = do
	us' <- mapM gl us
	ds' <- mapM gl ds
	return $ PNGImageL False bits us' ds'
	where
	gl ([], _) = fail "can't go left"
	gl (l : ls, rs) = return (ls, l : rs)
pngImageLRight (PNGImageL False bits us ds) = do
	us' <- mapM gr us
	ds' <- mapM gr ds
	return $ PNGImageL False bits us' ds'
	where
	gr (_, []) = fail "can't go right"
	gl (ls, r : rs) = return (r : ls, rs)

fromPNGImageL :: PNGImageL -> (PNGHeader, BSL.ByteString)
fromPNGImageL pil@(PNGImageL False 8 us ds) = (
	PNGHeader {
		pngWidth = pilWidth pil,
		pngHeight = pilHeight pil,
		pngDepth = 8,
		pngColorType = PNGTypeColor Nothing,
		pngCompType = 0,
		pngFilterType = 0,
		pngInterlaceType = 0,
		pngPalette = []
	 },
	BSL.pack $
		filterPNGImageL (replicate (w + 1) $ PNGImageLColor 0 0 0 65535)
			$ map (\(ls, rs) -> ls ++ rs) $ us ++ ds
--		concatMap (\(ls, rs) ->
--		0 : concatMap pilColorToWord8 (ls ++ rs)) $ us ++ ds
 )
	where
	w = case (us, ds) of
		((rs, ls) : _, _) -> length rs + length ls
		(_, (rs, ls) : _) -> length rs + length ls

pilWidth, pilHeight :: PNGImageL -> Int
pilWidth (PNGImageL False _ ((ls, rs) : _) _) = length ls + length rs
pilWidth (PNGImageL False _ _ ((ls, rs) : _)) = length ls + length rs
pilHeight (PNGImageL False _ us ds) = length us + length ds

filterPNGImageL :: [PNGImageLColor] -> [[PNGImageLColor]] -> [Word8]
filterPNGImageL _ [] = []
filterPNGImageL pre (l : ls) = 4 :
	filterLinePNGImageL pre (PNGImageLColor 0 0 0 65535) l ++
	filterPNGImageL (PNGImageLColor 0 0 0 65535 : l) ls

filterLinePNGImageL :: [PNGImageLColor] -> PNGImageLColor -> [PNGImageLColor] -> [Word8]
filterLinePNGImageL _ _ [] = []
filterLinePNGImageL pre left (px : pxs) =
	pilColorToWord8f 4 pre left px ++
	filterLinePNGImageL (tail pre) px pxs

pilColorToWord8f ::
	Int -> [PNGImageLColor] -> PNGImageLColor -> PNGImageLColor -> [Word8]
pilColorToWord8f 4 pre left px = let
	PNGImageLColor rlu glu blu 65535 : PNGImageLColor ru gu bu 65535 : _ = pre
	PNGImageLColor rl gl bl 65535 = left
	PNGImageLColor rpx gpx bpx 65535 = px in [
{-
		fi rpx - ((fi ru + fi rl) `div` 2),
		fi gpx - ((fi gu + fi gl) `div` 2),
		fi bpx - ((fi bu + fi bl) `div` 2)
-}
		fi rpx - paeth' (fi rl) (fi ru) (fi rlu),
		fi gpx - paeth' (fi gl) (fi gu) (fi glu),
		fi bpx - paeth' (fi bl) (fi bu) (fi blu)
	 ]
	where
	fi = fromIntegral . (`shiftR` 8)

pilColorToWord8 :: PNGImageLColor -> [Word8]
pilColorToWord8 (PNGImageLColor r g b 65535) = [fi r, fi g, fi b]
	where
	fi = fromIntegral . (`shiftR` 8)

makePNGImageL :: PNGHeader -> BSL.ByteString -> PNGImageL
makePNGImageL PNGHeader {
	pngWidth = w,
	pngHeight = h,
	pngDepth = 8,
	pngColorType = PNGTypeColor Nothing,
	pngCompType = 0,
	pngFilterType = 0,
	pngInterlaceType = 0
 } bs = PNGImageL False 8 [] $ map ([] ,) $ bsToImageA w bs

makePNGImageL PNGHeader {
	pngWidth = w,
	pngHeight = h,
	pngDepth = 2,
	pngColorType = PNGTypeIndex [],
	pngCompType = 0,
	pngFilterType = 0,
	pngInterlaceType = 0,
	pngPalette = p
 } bs = PNGImageL False 8 [] $ map ([] ,) $
	groupN w $ map (intsToPNGImageLColor . (p !!)) $ concatMap toIndexes2 $
		map BSL.tail $ groupNBS (w `div` 4 + 1) bs

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)

groupNBS :: Int -> BSL.ByteString -> [BSL.ByteString]
groupNBS n_ bs
	| BSL.null bs = []
	| otherwise = BSL.take n bs : groupNBS n_ (BSL.drop n bs)
	where
	n = fromIntegral n_

intsToPNGImageLColor :: (Int, Int, Int) -> PNGImageLColor
intsToPNGImageLColor = convert

toIndexes2 :: BSL.ByteString -> [Int]
toIndexes2 bs = concatMap word2Int2 $ BSL.unpack bs

word2Int2 :: Word8 -> [Int]
word2Int2 w = map fromIntegral [
	w `shiftR` 6, (w .&. 0x30) `shiftR` 4,
	(w .&. 0x0c) `shiftR` 2, w .&. 0x03]

bsToImageA :: Int -> BSL.ByteString -> [[PNGImageLColor]]
bsToImageA w = map (map convert) . bsToImage w

convert (r, g, b) = PNGImageLColor
	(toRGB16 r) (toRGB16 g) (toRGB16 b) (255 `shiftL` 8 .|. 255)

-- toRGB16 :: Word8 -> Int
toRGB16 w = fromIntegral w `shiftL` 8 .|. fromIntegral w

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
getColor 3 left up leftup rgb = zipWith3 (\l u p -> p + (l + u) `div` 2) left up rgb
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
