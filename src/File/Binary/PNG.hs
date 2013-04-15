module File.Binary.PNG (
	readPNG,
	writePNG,

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
import File.Binary (Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import Data.ByteString.Lazy as BSL (ByteString, toChunks, fromChunks, concat)
import Data.Monoid (mempty)
import Control.Monad (unless)
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
bplte = filter $ (`elem` beforePLTEs) . typeChunk
bidat = filter $ (`elem` beforeIDATs) . typeChunk
aplace = filter $ (`elem` anyplaces) . typeChunk
others = filter $ (`notElem` needs) . typeChunk
others' = filter $ (`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . typeChunk

readPNG :: Binary b => b -> Either String [Chunk]
readPNG b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "can't read whole binary"
	return $ map chunkData $ chunks p

writePNG :: Binary b => IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> b
writePNG i p cs b = writePNG' $ png i p cs b

writePNG' :: Binary b => [Chunk] -> b
writePNG' = toBinary () . PNG . map createChunk

png :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
png i (Just p) cs_ b = ChunkIHDR i : bplte cs_ ++ ChunkPLTE p : bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others' cs_ ++ [ChunkIEND IEND]
png i Nothing cs_ b = ChunkIHDR i : bplte cs_ ++ bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others' cs_ ++ [ChunkIEND IEND]
