{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.RW (
	PNG(..),
	Chunk(..),
	TypeChunk(..),
	typeChunk,

	IHDR,
	PLTE,
	IDAT(..),
	IEND(..),

	createChunk,
	chunkData,

	needs,
	beforeIDATs,
	beforePLTEs,
	anyplaces,
) where

import Language.Haskell.TH (
	mkName, instanceD, tySynInstD, cxt, funD, clause, normalB,
	conT, appT, tupleT, conP, varP, wildP, tupP,
	conE, varE, appE, appsE, infixApp)
import Control.Applicative ((<$>))
import Control.Arrow(first)
import Data.ByteString.Lazy (ByteString, append)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.PNG.CRC (crcb, checkCRC)
import File.Binary.PNG.Chunks (
	Chunk(..), TypeChunk(..), IHDR, PLTE, IDAT(..), IEND(..),
	typeChunk, name, needs, beforeIDATs, beforePLTEs, anyplaces,
	mkFromBinary, mkToBinary, chunkConsNames, chunkNamePairs)

--------------------------------------------------------------------------------

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

createChunk :: Chunk -> ChunkStructure
createChunk cb = ChunkStructure {
	chunkSize = length (toBinary (undefined, name cb) cb :: String),
	chunkName = name cb,
	chunkData = cb,
	chunkCRC = CRC }

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
