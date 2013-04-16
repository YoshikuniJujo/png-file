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
import File.Binary.PNG.RW
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import Data.ByteString.Lazy as BSL (ByteString, toChunks, fromChunks, concat)
import Data.List (find)

--------------------------------------------------------------------------------

body :: [Chunk] -> ByteString
body = decompress . concat . map idat_body . map (\(ChunkIDAT i) -> i) .
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

bplte, bidat, aplace, others, others' :: [Chunk] -> [Chunk]
bplte = filter $ (`elem` beforePLTE) . typeChunk
bidat = filter $ (`elem` beforeIDAT) . typeChunk
aplace = filter $ (`elem` anyPlace) . typeChunk
others = filter $ (`notElem` critical) . typeChunk
others' = filter $ (`notElem` critical ++ beforePLTE ++ beforeIDAT ++ anyPlace) . typeChunk

mkChunks :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
mkChunks i (Just p) cs_ b = ChunkIHDR i : bplte cs_ ++ ChunkPLTE p : bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others' cs_ ++ [ChunkIEND IEND]
mkChunks i Nothing cs_ b = ChunkIHDR i : bplte cs_ ++ bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others' cs_ ++ [ChunkIEND IEND]
