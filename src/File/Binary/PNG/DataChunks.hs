module File.Binary.PNG.DataChunks (
	module File.Binary.PNG.Chunks,
	makePNGHeader,
	bsToPNGImage,
	pngImageToBS,
	PNGImageL(..),
	PNGImageLColor(..)
) where

import File.Binary.PNG.Data
import File.Binary.PNG.Chunks
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.List

bsToPNGImage :: PNGImage pi =>
	IHDR -> Maybe TRNS -> BSL.ByteString -> Either String pi
bsToPNGImage ihdr trns bs = do
	h <- makePNGHeader ihdr trns
	return $ makePNGImage h bs

pngImageToBS :: PNGImage pi => pi -> (IHDR, Maybe TRNS, BSL.ByteString)
pngImageToBS pi = let
	(header, bs) = fromPNGImage pi
	(ihdr, trns) = fromPNGHeader header in
	(ihdr, trns, bs)

makePNGHeader :: IHDR -> Maybe TRNS -> Either String PNGHeader
makePNGHeader ihdr trns = do
	ct <- getColorType (alpha ihdr) (color ihdr) (palet ihdr) trns
	return PNGHeader {
		pngWidth = width ihdr,
		pngHeight = height ihdr,
		pngDepth = depth ihdr,
		pngColorType = ct,
		pngCompType = compressionType ihdr,
		pngFilterType = filterType ihdr,
		pngInterlaceType = interlaceType ihdr
	 }

fromPNGHeader :: PNGHeader -> (IHDR, Maybe TRNS)
fromPNGHeader ph@PNGHeader { pngColorType = PNGTypeColor Nothing } = (IHDR {
	width = pngWidth ph,
	height = pngHeight ph,
	depth = pngDepth ph,
	alpha = False,
	color = True,
	palet = False,
	compressionType = 0,
	filterType = 0,
	interlaceType = 0 }, Nothing)
getTRNSChunk :: [Chunk] -> Maybe Chunk
getTRNSChunk = find ((== T_tRNS) . typeChunk)

getColorType :: Bool -> Bool -> Bool -> Maybe TRNS -> Either String PNGColorType
getColorType False True True = return . PNGTypeIndex . maybe [] readTRNSi
getColorType False False False = return . PNGTypeGrey . fmap readTRNSg
getColorType False True False = return . PNGTypeColor . fmap readTRNSc
getColorType True False False = const $ return PNGTypeGreyAlpha
getColorType True True False = const $ return PNGTypeColorAlpha
getColorType _ _ _ = fail "bad colortype"

readTRNSi :: TRNS -> [Int]
readTRNSi = map fromIntegral . BSL.unpack . dat

readTRNSg :: TRNS -> Int
readTRNSg t = let [h, l] = map fromIntegral $ BSL.unpack $ dat t in
	h `shiftL` 8 .|. l

readTRNSc :: TRNS -> (Int, Int, Int)
readTRNSc t = let
	[rh, rl, gh, gl, bh, bl] = map fromIntegral $ BSL.unpack $ dat t in (
		rh `shiftL` 8 .|. rl,
		gh `shiftL` 8 .|. gl,
		bh `shiftL` 8 .|. bl)
