{-# LANGUAGE
	QuasiQuotes,
	TypeFamilies,
	FlexibleInstances,
	OverloadedStrings,
	ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.RW (
	PNG,
	chunks,
	chunkData,
	chunkName,

	readBinaryFile,

	readPNG,
	ihdr,
	bplte,
	plte,
	bidat,
	body,
	aplace,
	others,

	writePNG,
	writePNG',
	IHDR(..),
	PLTE(..),
	png,
	otherChunks
) where

import Prelude hiding (concat)
import File.Binary (binary, Field(..), Binary(..), readBinaryFile)
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks
import File.Binary.PNG.CRC (crcb, checkCRC)
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import qualified Data.ByteString as BS (ByteString, length, writeFile)
import Data.ByteString.Lazy as BSL
	(ByteString, pack, unpack, concat, toChunks, fromChunks, append)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Monoid (mempty)
import Data.List (find)
import Control.Monad (unless)
import Control.Arrow(first)

--------------------------------------------------------------------------------

ihdr :: [Chunk] -> IHDR
ihdr = (\(ChunkIHDR i) -> i) . head . filter isIHDR

bplte :: [ChunkStructure] -> [ChunkStructure]
bplte = filter ((`elem` beforePLTEs) . chunkName)

plte :: [Chunk] -> Maybe PLTE
plte c = do
	ChunkPLTE pl <- find isPLTE c
	return pl

bidat :: [ChunkStructure] -> [ChunkStructure]
bidat = filter ((`elem` beforeIDATs) . chunkName)

body :: [Chunk] -> ByteString
body = decompress . concatIDATs . body'

aplace :: [ChunkStructure] -> [ChunkStructure]
aplace = filter ((`elem` anyplaces) . chunkName)

others :: [ChunkStructure] -> [ChunkStructure]
others = filter $
	(`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . chunkName

needs :: [ByteString]
needs = ["IHDR", "PLTE", "IDAT", "IEND"]

beforePLTEs :: [ByteString]
beforePLTEs = ["cHRM", "gAMA", "sBIT", "sRGB", "iCCP"]

-- elemChunks :: [ByteString] -> [

beforeIDATs :: [ByteString]
beforeIDATs = ["bKGD", "hIST", "tRNS", "pHYs", "sPLT", "oFFs", "pCAL", "sCAL"]

anyplaces :: [ByteString]
anyplaces = ["tIME", "tEXt", "zTXt", "iTXt", "gIFg", "gIFt", "gIFx", "fRAc"]

otherChunks :: [Chunk] -> [Chunk]
otherChunks = filter (not . isNeed)

readPNG :: Binary b => b -> Either String [Chunk]
readPNG b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "can't read whole binary"
	return $ map chunkData $ chunks p

writePNG' :: Binary b => [Chunk] -> b
writePNG' = toBinary () . PNG . map cToC

writePNG :: FilePath -> PNG -> IO ()
writePNG fout = BS.writeFile fout . toBinary ()

mkBody :: ByteString -> [ChunkStructure]
mkBody = map makeIDAT . toChunks . compressWith defaultCompressParams {
		compressLevel = bestCompression,
		compressWindowBits = WindowBits 10
	 }

{-
makeChunks :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> [Chunk]
makeChunks i (Just p) cs b =
	ChunkIHDR i :  ChunkPLTE p : cs 
-}

png :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> PNG
png i (Just p) cs_ b = let cs = map cToC cs_ in
	PNG { chunks = makeIHDR i : bplte cs ++ makePLTE p : bidat cs ++
		mkBody b ++ aplace cs ++ others cs ++ [iend] }
png i Nothing cs_ b = let cs = map cToC cs_ in
	PNG { chunks = makeIHDR i : bplte cs ++ bidat cs ++
		mkBody b ++ aplace cs ++ others cs ++ [iend] }

cToC :: Chunk -> ChunkStructure
cToC = makeChunk

makeChunk :: Chunk -> ChunkStructure
makeChunk cb = ChunkStructure {
	chunkSize = length (toBinary (size cb, name cb) cb :: String),
	chunkName = name cb,
	chunkData = cb,
	chunkCRC = CRC }

body' :: [Chunk] -> [IDAT]
body' cs = map cidat $ filter isIDAT cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat

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

makeIDAT :: BS.ByteString -> ChunkStructure
makeIDAT bs = ChunkStructure {
	chunkSize = fromIntegral $ BS.length bs,
	chunkName = "IDAT",
	chunkData = ChunkIDAT $ IDAT $ fromChunks [bs],
	chunkCRC = CRC }

makeIHDR :: IHDR -> ChunkStructure
makeIHDR = makeChunk . ChunkIHDR

makePLTE :: PLTE -> ChunkStructure
makePLTE = makeChunk . ChunkPLTE

size :: Chunk -> Int
size (ChunkIHDR _) = 13
size (ChunkPLTE (PLTE d)) = 3 * length d
size _ = error "yet"

iend :: ChunkStructure
iend = ChunkStructure {
	chunkSize = 0,
	chunkName = "IEND",
	chunkData = ChunkIEND IEND,
	chunkCRC = CRC } -- crcb "IEND" }

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
