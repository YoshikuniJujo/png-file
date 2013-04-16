module File.Binary.PNG (
	getChunks,
	putChunks,

	mkChunks,
	ihdr,
	plte,
	body,
	others
) where

import Prelude hiding (concat)
import File.Binary.PNG.Chunks
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import Data.ByteString.Lazy as BSL (ByteString, toChunks, fromChunks, concat)
import Data.List (find)

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
ihdr = (\(ChunkIHDR i) -> i) . head . filter ((== T_IHDR) . typeChunk)

plte :: [Chunk] -> Maybe PLTE
plte c = do
	ChunkPLTE pl <- find ((== T_PLTE) . typeChunk) c
	return pl

others :: [Chunk] -> [Chunk]
others = filter $ (`notElem` [T_IHDR, T_PLTE, T_IDAT, T_IEND]) . typeChunk

mkChunks :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
mkChunks i (Just p) cs b = ChunkIHDR i : ChunkPLTE p : mkBody b ++ cs
mkChunks i Nothing cs b = ChunkIHDR i : mkBody b ++ cs
