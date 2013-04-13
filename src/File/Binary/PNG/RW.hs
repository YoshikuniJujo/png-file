{-# LANGUAGE
	QuasiQuotes,
	TypeFamilies,
	FlexibleInstances,
	OverloadedStrings,
	ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.RW (
	readBinaryFile,
	writeBinaryFile,

	readPNG,
	writePNG,

	png,
	ihdr,
	plte,
	body,
	otherChunks,

--	IHDR(..),
--	PLTE(..),
) where

import File.Binary (binary, Field(..), Binary(..), readBinaryFile, writeBinaryFile)
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks
import File.Binary.PNG.CRC (crcb, checkCRC)
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import qualified Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as BSL
	(ByteString, pack, unpack, toChunks, fromChunks, append)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Monoid (mempty)
import Control.Monad (unless)
import Control.Arrow(first)

--------------------------------------------------------------------------------

body :: [Chunk] -> ByteString
body = decompress . idats

otherChunks :: [Chunk] -> [Chunk]
otherChunks = filter (not . isNeed)

readPNG :: Binary b => b -> Either String [Chunk]
readPNG b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "can't read whole binary"
	return $ map chunkData $ chunks p

writePNG :: Binary b => [Chunk] -> b
writePNG = toBinary () . PNG . map cToC

mkBody :: ByteString -> [Chunk]
mkBody = map (mkIDAT) . toChunks . compressWith defaultCompressParams {
		compressLevel = bestCompression,
		compressWindowBits = WindowBits 10
	 }

png :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
png i (Just p) cs_ b = ChunkIHDR i : bplte cs_ ++ ChunkPLTE p : bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others cs_ ++ [ChunkIEND IEND]
png i Nothing cs_ b = ChunkIHDR i : bplte cs_ ++ bidat cs_ ++
	mkBody b ++ aplace cs_ ++ others cs_ ++ [ChunkIEND IEND]

cToC :: Chunk -> ChunkStructure
cToC = makeChunk

makeChunk :: Chunk -> ChunkStructure
makeChunk cb = ChunkStructure {
	chunkSize = length (toBinary (size cb, name cb) cb :: String),
	chunkName = name cb,
	chunkData = cb,
	chunkCRC = CRC }

isIDAT :: Chunk -> Bool
isIDAT ChunkIDAT {} = True
isIDAT _ = False

isIHDR :: Chunk -> Bool
isIHDR ChunkIHDR {} = True
isIHDR _ = False

isPLTE :: Chunk -> Bool
isPLTE ChunkPLTE {} = True
isPLTE _ = False

isIEND :: Chunk -> Bool
isIEND ChunkIEND {} = True
isIEND _ = False

isNeed :: Chunk -> Bool
isNeed c = or [isIDAT c, isIHDR c, isPLTE c, isIEND c]

mkIDAT :: BS.ByteString -> Chunk
mkIDAT = ChunkIDAT . IDAT . fromChunks . (: [])

size :: Chunk -> Int
size (ChunkIHDR _) = 13
size (ChunkPLTE (PLTE d)) = 3 * length d
size (ChunkIEND _) = 0
size _ = error "size: yet"

[binary|

PNG deriving Show

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"
((), Nothing){[ChunkStructure]}: chunks

|]

[binary|

ChunkStructure deriving Show

4: chunkSize
4{ByteString}: chunkName
(chunkSize, chunkName){Chunk}: chunkData
(chunkName, chunkData, (chunkSize, chunkName)){CRC}: chunkCRC

|]

data CRC = CRC deriving Show

instance Field CRC where
	type FieldArgument CRC = (ByteString, Chunk, (Int, ByteString))
	fromBinary (nam, bod, arg) b =
		if checkCRC (nam `append` toBinary arg bod) bs
			then return (CRC, rest)
			else fail "bad crc"b
		where
		(bs, rest) = getBytes 4 b
	toBinary (nam, bod, arg) _ = makeBinary $ crcb $ nam `append` toBinary arg bod

instance Field Word32 where
	type FieldArgument Word32 = Int
	toBinary n = makeBinary . pack . intToWords n
	fromBinary n = return . first (wordsToInt . unpack) . getBytes n

intToWords :: (Bits i, Integral i) => Int -> i -> [Word8]
intToWords = itw []
	where
	itw r 0 _ = r
	itw r n i = itw (fromIntegral (i .&. 0xff) : r) (n - 1) (i `shiftR` 8)

wordsToInt :: Bits i => [Word8] -> i
wordsToInt = foldl (\r w -> r `shiftL` 8 .|. fromIntegral w) 0

instance Field Chunk where
	type FieldArgument Chunk = (Int, ByteString)
	toBinary _ (ChunkIHDR c) = toBinary () c
	toBinary _ (ChunkGAMA c) = toBinary () c
	toBinary _ (ChunkSRGB c) = toBinary () c
	toBinary (n, _) (ChunkCHRM chrm) = toBinary n chrm
	toBinary (n, _) (ChunkPLTE p) = toBinary n p
	toBinary _ (ChunkBKGD c) = toBinary () c
	toBinary (n, _) (ChunkIDAT c) = toBinary n c
	toBinary (n, _) (ChunkTEXT c) = toBinary n c
	toBinary _ (ChunkIEND c) = toBinary () c
	toBinary (n, _) (Others _ str) = toBinary n str
	fromBinary (_, "IHDR") = fmap (first ChunkIHDR) . fromBinary ()
	fromBinary (_, "gAMA") = fmap (first ChunkGAMA) . fromBinary ()
	fromBinary (_, "sRGB") = fmap (first ChunkSRGB) . fromBinary ()
	fromBinary (n, "cHRM") = fmap (first ChunkCHRM) . fromBinary n
	fromBinary (n, "PLTE") = fmap (first ChunkPLTE) . fromBinary n
	fromBinary (_, "bKGD") = fmap (first ChunkBKGD) . fromBinary ()
	fromBinary (n, "IDAT") = fmap (first ChunkIDAT) . fromBinary n
	fromBinary (n, "tEXt") = fmap (first ChunkTEXT) . fromBinary n
	fromBinary (_, "IEND") = fmap (first ChunkIEND) . fromBinary ()
	fromBinary (n, nam) = fmap (first $ Others nam) . fromBinary n
