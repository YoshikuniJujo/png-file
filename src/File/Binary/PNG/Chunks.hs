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

import Language.Haskell.TH (
	mkName, instanceD, cxt, tySynInstD, funD, clause, normalB,
	conT, appT, tupleT, conP, varP, wildP, tupP,
	conE, varE, appE, appsE, infixApp)
import Control.Applicative ((<$>))
import Control.Arrow(first)
import Control.Monad (unless)
import Data.Monoid (mempty)
import Data.ByteString.Lazy (ByteString, append)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks.CRC (crc, checkCRC)
import File.Binary.PNG.Chunks.Chunks (
	Chunk(..), TypeChunk(..), typeChunk, getName,
	mkFromBinary, mkToBinary, chunkConsNames, chunkNamePairs,
	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS, CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..), ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME,
	beforePLTE, beforeIDAT, anyPlace)

--------------------------------------------------------------------------------

getChunks :: Binary b => b -> Either String [Chunk]
getChunks b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "couldn't read whole binary"
	return $ map chunkData $ chunks p

putChunks :: Binary b => [Chunk] -> b
putChunks = toBinary () . PNGFile . map createChunk . sortChunks

filterChunks :: [TypeChunk] -> [Chunk] -> [Chunk]
filterChunks ts = filter $ (`elem` ts) . typeChunk

sortChunks :: [Chunk] -> [Chunk]
sortChunks cs = concatMap (($ cs) . filterChunks)
	[[T_IHDR], beforePLTE, [T_PLTE], beforeIDAT, [T_IDAT], anyPlace, [T_IEND]]

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

createChunk :: Chunk -> ChunkStructure
createChunk cb = ChunkStructure {
	chunkSize = length (toBinary (undefined, name) cb :: String),
	chunkName = name,
	chunkData = cb,
	chunkCRC = CRC }
	where
	name = getName $ typeChunk cb

data CRC = CRC deriving Show

instance Field CRC where
	type FieldArgument CRC = (ByteString, Chunk, (Int, ByteString))
	fromBinary (nam, bod, arg) b = let (bs, rest) = getBytes 4 b in
		if checkCRC (nam `append` toBinary arg bod) bs
			then return (CRC, rest)
			else fail "bad crc"
	toBinary (nam, bod, arg) _ =
		makeBinary $ crc $ nam `append` toBinary arg bod

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
