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
	chunkName,

	readPNG,
	ihdr,
	bplte,
	plte,
	bidat,
	body,
	aplace,
	others,

	writePNG,
	IHDR(..),
	PLTE(..),
	png,
	otherChunks
) where

import Prelude hiding (concat)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.CRC (crcb, checkCRC)
import Codec.Compression.Zlib (
	decompress, compressWith, defaultCompressParams, CompressParams(..),
	bestCompression, WindowBits(..))
import qualified Data.ByteString as BS (ByteString, length, readFile, writeFile)
import Data.ByteString.Lazy as BSL
	(ByteString, pack, unpack, concat, toChunks, fromChunks, append)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Monoid (mconcat)
import Data.List (find)
import Control.Applicative ((<$>))
import Control.Arrow(first)

--------------------------------------------------------------------------------

readPNG :: FilePath -> IO PNG
readPNG fp = do
	Right (p, "") <- fromBinary () <$> BS.readFile fp
	return p

writePNG :: FilePath -> PNG -> IO ()
writePNG fout = BS.writeFile fout . toBinary ()

body :: PNG -> ByteString
body = decompress . concatIDATs . body'

ihdr :: PNG -> IHDR
ihdr p = let
	ChunkIHDR i = chunkData . head . filter ((== "IHDR") . chunkName) $ chunks p
	in i

plte :: PNG -> Maybe PLTE
plte p = do
	ChunkPLTE pl <- chunkData <$> find ((== "PLTE") . chunkName) (chunks p)
	return pl

mkBody :: ByteString -> [Chunk]
mkBody = map makeIDAT . toChunks . compressWith defaultCompressParams {
		compressLevel = bestCompression,
		compressWindowBits = WindowBits 10
	 }

png :: IHDR -> Maybe PLTE -> [Chunk] -> ByteString -> PNG
png i (Just p) cs b =
	PNG { chunks = makeIHDR i : bplte cs ++ makePLTE p : bidat cs ++
		mkBody b ++ aplace cs ++ others cs ++ [iend] }
png i Nothing cs b =
	PNG { chunks = makeIHDR i : bplte cs ++ bidat cs ++
		mkBody b ++ aplace cs ++ others cs ++ [iend] }

beforePLTEs :: [ByteString]
beforePLTEs = ["cHRM", "gAMA", "sBIT", "sRGB", "iCCP"]

bplte :: [Chunk] -> [Chunk]
bplte = filter ((`elem` beforePLTEs) . chunkName)

beforeIDATs :: [ByteString]
beforeIDATs = ["bKGD", "hIST", "tRNS", "pHYs", "sPLT", "oFFs", "pCAL", "sCAL"]

bidat :: [Chunk] -> [Chunk]
bidat = filter ((`elem` beforeIDATs) . chunkName)

anyplaces :: [ByteString]
anyplaces = ["tIME", "tEXt", "zTXt", "iTXt", "gIFg", "gIFt", "gIFx", "fRAc"]

aplace :: [Chunk] -> [Chunk]
aplace = filter ((`elem` anyplaces) . chunkName)

needs :: [ByteString]
needs = ["IHDR", "PLTE", "IDAT", "IEND"]

otherChunks :: PNG -> [Chunk]
otherChunks = filter ((`notElem` needs) . chunkName) . chunks

others :: [Chunk] -> [Chunk]
others = filter $
	(`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . chunkName

body' :: PNG -> [IDAT]
body' PNG { chunks = cs } =
	map (cidat . chunkData) $ filter ((== "IDAT") . chunkName) cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat

makeIDAT :: BS.ByteString -> Chunk
makeIDAT bs = Chunk {
	chunkSize = fromIntegral $ BS.length bs,
	chunkName = "IDAT",
	chunkData = ChunkIDAT $ IDAT $ fromChunks [bs],
	chunkCRC = CRC }

makeIHDR :: IHDR -> Chunk
makeIHDR = makeChunk . ChunkIHDR

makePLTE :: PLTE -> Chunk
makePLTE = makeChunk . ChunkPLTE

size :: ChunkBody -> Int
size (ChunkIHDR _) = 13
size (ChunkPLTE (PLTE d)) = 3 * length d
size _ = error "yet"

name :: ChunkBody -> ByteString
name (ChunkIHDR _) = "IHDR"
name (ChunkPLTE _) = "PLTE"
name _ = error "yet"

makeChunk :: ChunkBody -> Chunk
makeChunk cb = Chunk {
	chunkSize = length (toBinary (size cb, name cb) cb :: String),
	chunkName = name cb,
	chunkData = cb,
--	chunkCRC = CRC "" } -- crcb $ name cb `append` toBinary (size cb, name cb) cb }
	chunkCRC = CRC }

iend :: Chunk
iend = Chunk {
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
((), Nothing){[Chunk]}: chunks

|]

[binary|

Chunk deriving Show

4: chunkSize
4{ByteString}: chunkName
(chunkSize, chunkName){ChunkBody}: chunkData
(chunkName, chunkData, (chunkSize, chunkName)){CRC}: chunkCRC

|]

data CRC = CRC deriving Show

instance Field CRC where
	type FieldArgument CRC = (ByteString, ChunkBody, (Int, ByteString))
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

data ChunkBody
	= ChunkIHDR IHDR
	| ChunkGAMA GAMA
	| ChunkSRGB SRGB
	| ChunkCHRM CHRM
	| ChunkPLTE PLTE
	| ChunkBKGD BKGD
	| ChunkIDAT { cidat :: IDAT }
	| ChunkTEXT TEXT
	| ChunkIEND IEND
	| Others String
	deriving Show

instance Field ChunkBody where
	type FieldArgument ChunkBody = (Int, ByteString)
	toBinary _ (ChunkIHDR c) = toBinary () c
	toBinary _ (ChunkGAMA c) = toBinary () c
	toBinary _ (ChunkSRGB c) = toBinary () c
	toBinary (n, _) (ChunkCHRM chrm) = toBinary n chrm
	toBinary (n, _) (ChunkPLTE p) = toBinary n p
	toBinary _ (ChunkBKGD c) = toBinary () c
	toBinary (n, _) (ChunkIDAT c) = toBinary n c
	toBinary (n, _) (ChunkTEXT c) = toBinary n c
	toBinary _ (ChunkIEND c) = toBinary () c
	toBinary (n, _) (Others str) = toBinary ((), Just n) str
	fromBinary (_, "IHDR") = fmap (first ChunkIHDR) . fromBinary ()
	fromBinary (_, "gAMA") = fmap (first ChunkGAMA) . fromBinary ()
	fromBinary (_, "sRGB") = fmap (first ChunkSRGB) . fromBinary ()
	fromBinary (n, "cHRM") = fmap (first ChunkCHRM) . fromBinary n
	fromBinary (n, "PLTE") = fmap (first ChunkPLTE) . fromBinary n
	fromBinary (_, "bKGD") = fmap (first ChunkBKGD) . fromBinary ()
	fromBinary (n, "IDAT") = fmap (first ChunkIDAT) . fromBinary n
	fromBinary (n, "tEXt") = fmap (first ChunkTEXT) . fromBinary n
	fromBinary (_, "IEND") = fmap (first ChunkIEND) . fromBinary ()
	fromBinary (n, _) = fmap (first Others) . fromBinary ((), Just n)

[binary|

IHDR deriving Show

4: width
4: height
1: depth
: False
: False
: False
: False
: False
{Bool}: alpha
{Bool}: color
{Bool}: palet
1: compressionType
1: filterType
1: interlaceType

|]

[binary|

GAMA deriving Show

4: gamma

|]

[binary|

SRGB deriving Show

1: srgb

|]

[binary|

CHRM

deriving Show

arg :: Int

(4, Just (arg `div` 4)){[Int]}: chrms

|]

[binary|

PLTE deriving Show

arg :: Int

((), Just (arg `div` 3)){[(Int, Int, Int)]}: colors

|]

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	toBinary _ (b, g, r) = mconcat [toBinary 1 b, toBinary 1 g, toBinary 1 r]
	fromBinary _ s = do
		(r, rest) <- fromBinary 1 s
		(g, rest') <- fromBinary 1 rest
		(b, rest'') <- fromBinary 1 rest'
		return ((r, g, b), rest'')

[binary|

BKGD deriving Show

1: bkgd

|]

[binary|

IDAT deriving Show

arg :: Int

arg{ByteString}: idat

|]

[binary|

TEXT deriving Show

arg :: Int

((), Just arg){String}: text

|]

[binary|IEND deriving Show|]
