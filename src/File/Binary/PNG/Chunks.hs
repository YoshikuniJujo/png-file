{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.Chunks (
	Chunk(..),
	TypeChunk(..),
	typeChunk,

	getChunks,
	putChunks,

	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS,
	CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..),
	ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT,
	TIME
	
) where

import Prelude hiding (length)
import Control.Monad (unless)
import Data.Monoid (mempty)
import Data.ByteString.Lazy (ByteString, append, length)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks.CRC (crc, checkCRC)
import File.Binary.PNG.Chunks.Data (
	Chunk(..), TypeChunk(..), typeChunk, typeToName, instanceFieldChunk,
	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS, CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..), ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME, bplte, bidat, aplace)

--------------------------------------------------------------------------------

getChunks :: Binary b => b -> Either String [Chunk]
getChunks b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "couldn't read whole binary"
	return $ map chunkData $ chunks p

putChunks :: Binary b => [Chunk] -> b
putChunks = toBinary () . PNGFile . map createChunk . sortChunks

createChunk :: Chunk -> ChunkStructure
createChunk cb = ChunkStructure {
	chunkSize = fromIntegral $ length $ toBinary (undefined, name) cb,
	chunkName = name,
	chunkData = cb,
	chunkCRC = CRC }
	where
	name = typeToName $ typeChunk cb

filterChunks :: [TypeChunk] -> [Chunk] -> [Chunk]
filterChunks ts = filter $ (`elem` ts) . typeChunk

sortChunks :: [Chunk] -> [Chunk]
sortChunks cs = concatMap (($ cs) . filterChunks)
	[[T_IHDR], bplte, [T_PLTE], bidat, [T_IDAT], aplace, [T_IEND]]

[binary|

PNGFile deriving Show

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

instanceFieldChunk

instance Field CRC where
	type FieldArgument CRC = (ByteString, Chunk, (Int, ByteString))
	fromBinary (nam, bod, arg) b = let (bs, rest) = getBytes 4 b in
		if checkCRC (nam `append` toBinary arg bod) bs
			then return (CRC, rest)
			else fail "bad crc"
	toBinary (nam, bod, arg) _ =
		makeBinary $ crc $ nam `append` toBinary arg bod
