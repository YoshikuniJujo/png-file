module File.Binary.PNG (
	module File.Binary.PNG.RW,
	readPNG,
	otherChunks,
	body,
	png,
	writePNG
) where

import File.Binary.PNG.RW
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import File.Binary (Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks
import Data.ByteString.Lazy as BSL (ByteString, toChunks)
import Data.Monoid (mempty)
import Control.Monad (unless)

body :: [Chunk] -> ByteString
body = decompress . idats

otherChunks :: [Chunk] -> [Chunk]
otherChunks = filter $ (`notElem` needs) . typeChunk

readPNG :: Binary b => b -> Either String [Chunk]
readPNG b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "can't read whole binary"
	return $ map chunkData $ chunks p

writePNG :: Binary b => [Chunk] -> b
writePNG = toBinary () . PNG . map chunkToChunkStructure

mkBody :: ByteString -> [Chunk]
mkBody = map mkIDAT . toChunks . compressWith defaultCompressParams {
		compressLevel = bestCompression,
		compressWindowBits = WindowBits 10
	 }

png :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
png i (Just p) cs_ b = ChunkIHDR i : bplte cs_ ++ ChunkPLTE p : bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others cs_ ++ [ChunkIEND IEND]
png i Nothing cs_ b = ChunkIHDR i : bplte cs_ ++ bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others cs_ ++ [ChunkIEND IEND]
