{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}

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

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (unless)
import Data.Monoid (mempty)
import Data.List (isPrefixOf)
import Data.ByteString.Lazy (ByteString, append)
import qualified Data.ByteString.Lazy as BSL (length)
import Language.Haskell.TH (
	newName, nameBase, litP, stringL,
	cxt, instanceD, tySynInstD, clause, normalB,
	conT, appT, conP, varP, wildP, tupP, conE, varE, appE, appsE, infixApp)
import Language.Haskell.TH.Tools (wrapTypes, makeTypes, nameTypes, mapTypesFun)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.Chunks.CRC (crc, checkCRC)
import File.Binary.PNG.Chunks.Each (
	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS, CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..), ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME,
	chunkNames, beforePLTE, beforeIDAT, anyPlace)


--------------------------------------------------------------------------------

wrapTypes "Chunk" chunkNames ("ChunkOthers", [''ByteString, ''ByteString]) [''Show]
makeTypes "TypeChunk" ''Chunk "Chunk" "T_"
nameTypes ''TypeChunk "T_" 'T_Others ''ByteString

(:[]) <$> do
	let	removePrefix prefix str
			| prefix `isPrefixOf` str = drop (length prefix) str
			| otherwise = str
	instanceD (cxt []) (conT ''Field `appT` conT ''Chunk) [
		tySynInstD ''FieldArgument [conT ''Chunk] [t| (Int, ByteString) |],
		mapTypesFun 'fromBinary ''Chunk $ \con _ -> do
			[n, typ] <- mapM newName ["n", "typ"]
			let (t, c) = if con /= 'ChunkOthers
				then (litP $ stringL $ removePrefix "Chunk" $
					nameBase con, conE con)
				else (varP typ, conE con `appE` varE typ)
			flip (clause [tupP [varP n, t]]) [] $ normalB $ infixApp
				(varE 'fmap `appE` (varE 'first `appE` c))
				(varE '(.))
				(varE 'fromBinary `appE` varE n),
		mapTypesFun 'toBinary ''Chunk $ \con _ -> do
			[n, dat] <- mapM newName ["n", "dat"]
			let d = conP con $ if con /= 'ChunkOthers
				then [varP dat] else [wildP, varP dat]
			flip (clause [tupP [varP n, wildP], d]) [] $ normalB $
				appsE [varE 'toBinary, varE n, varE dat]]

bplte, bidat, aplace :: [TypeChunk]
[bplte, bidat, aplace] =
	map (map nameToTypeChunk) [beforePLTE, beforeIDAT, anyPlace]

getChunks :: Binary b => b -> Either String [Chunk]
getChunks b = do
	(p, rest) <- fromBinary () b
	unless (rest == mempty) $ fail "couldn't read whole binary"
	return $ map chunkData $ chunks p

putChunks :: Binary b => [Chunk] -> b
putChunks = toBinary () . PNGFile . map createChunk . sortChunks

createChunk :: Chunk -> ChunkStructure
createChunk cd = let name = typeChunkToName $ typeChunk cd in ChunkStructure {
	chunkSize = fromIntegral $ BSL.length $ toBinary (undefined, name) cd,
	chunkName = name,
	chunkData = cd,
	chunkCRC = CRC }

sortChunks :: [Chunk] -> [Chunk]
sortChunks cs = concatMap (($ cs) . filterChunks)
	[[T_IHDR], bplte, [T_PLTE], bidat, [T_IDAT], aplace, [T_IEND]]
	where
	filterChunks ts = filter $ (`elem` ts) . typeChunk

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

instance Field CRC where
	type FieldArgument CRC = (ByteString, Chunk, (Int, ByteString))
	fromBinary (name, body, arg) b = let (bs, rest) = getBytes 4 b in
		if checkCRC (name `append` toBinary arg body) bs
			then return (CRC, rest)
			else fail "bad crc"
	toBinary (name, body, arg) _ =
		makeBinary $ crc $ name `append` toBinary arg body
