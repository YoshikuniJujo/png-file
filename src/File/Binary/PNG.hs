module File.Binary.PNG (
	getChunks,
	putChunks,

	mkChunks,
	ihdr, IHDR(..),
	plte,
	body,
	others,

	TypeChunk(..),
	typeChunk,
	Chunk(..),
	makePNGHeader,
	bsToPNGImage,
	pngImageToBS,
	PNGImageL(..),
	PNGImageLColor(..)
) where

import Prelude hiding (concat)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy (ByteString, toChunks, fromChunks, concat)
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import File.Binary.PNG.DataChunks (
	Chunk(..), TypeChunk(..), typeChunk, IHDR(..), PLTE, IDAT(..),
	getChunks, putChunks, makePNGHeader, bsToPNGImage, PNGImageL(..),
	PNGImageLColor(..), pngImageToBS)

--------------------------------------------------------------------------------

body :: [Chunk] -> ByteString
body = decompress . concat . map (idat_body . (\(ChunkIDAT i) -> i)) .
	filter ((== T_IDAT) . typeChunk)

mkBody :: ByteString -> [Chunk]
mkBody = map (ChunkIDAT . IDAT . fromChunks . (: [])) . toChunks .
	compressWith defaultCompressParams {
		compressLevel = bestCompression,
		compressWindowBits = WindowBits 10
	 }

ihdr :: [Chunk] -> IHDR
ihdr = (\(ChunkIHDR i) -> i) . fromJust . find ((== T_IHDR) . typeChunk)

plte :: [Chunk] -> Maybe PLTE
plte c = do
	ChunkPLTE pl <- find ((== T_PLTE) . typeChunk) c
	return pl

others :: [Chunk] -> [Chunk]
others = filter $ (`notElem` [T_IHDR, T_PLTE, T_IDAT, T_IEND]) . typeChunk

mkChunks :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
mkChunks i (Just p) cs b = ChunkIHDR i : ChunkPLTE p : mkBody b ++ cs
mkChunks i Nothing cs b = ChunkIHDR i : mkBody b ++ cs
