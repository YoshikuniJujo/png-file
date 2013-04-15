{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.RW (
	TypeChunk(..),
	typeChunk,
	readBinaryFile,
	writeBinaryFile,

	Chunk,

	ihdr,
	plte,

	chunkToChunkStructure,
	mkIDAT,
	chunkData,
	PNG(..),
	needs
) where

import File.Binary (binary, Field(..), Binary(..), readBinaryFile, writeBinaryFile)
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks
import File.Binary.PNG.CRC (crcb, checkCRC)
import qualified Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as BSL
	(ByteString, pack, unpack, fromChunks, append)
import Data.Word (Word8, Word32)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Control.Applicative ((<$>))
import Control.Arrow(first)
import Language.Haskell.TH

--------------------------------------------------------------------------------

chunkToChunkStructure :: Chunk -> ChunkStructure
chunkToChunkStructure cb = ChunkStructure {
	chunkSize = length (toBinary (undefined, name cb) cb :: String),
	chunkName = name cb,
	chunkData = cb,
	chunkCRC = CRC }

mkIDAT :: BS.ByteString -> Chunk
mkIDAT = ChunkIDAT . IDAT . fromChunks . (: [])

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

(: []) <$> instanceD (cxt []) (conT ''Field `appT` conT ''Chunk) [
	tySynInstD ''FieldArgument [conT ''Chunk]
		(tupleT 2 `appT` conT ''Int `appT` conT ''ByteString),
	funD 'toBinary $ map mkToBinary chunkConsNames ++ [clause
		[tupP [varP $ mkName "n", wildP],
			conP 'Others [wildP, varP $ mkName "str"]]
		(normalB $ appsE
			[varE 'toBinary, varE $ mkName "n", varE $ mkName "str"])
		[]
	 ],
	funD 'fromBinary $ map (uncurry mkFromBinary) chunkNamePairs ++ [clause
		[tupP [varP $ mkName "n", varP $ mkName "nam"]]
		(normalB $ infixApp
			(varE 'fmap `appE` (varE 'first `appE`
				(conE 'Others `appE` varE (mkName "nam"))))
			(varE '(.))
			(varE 'fromBinary `appE` varE (mkName "n")))
		[]
	 ]
 ]
