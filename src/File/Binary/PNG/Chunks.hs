{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.Chunks (
	Chunk(..),
	TypeChunk(..),
	IHDR(..),
	CHRM(..),
	GAMA(..),
	SRGB(..),
	PLTE(..),
	BKGD(..),
	IDAT(..),
	TEXT(..),
	IEND(..),
	typeChunk,
	name,
	ihdr,
	bplte,
	plte,
	bidat,
	idats,
	aplace,
	others,

	mkToBinary,
	mkFromBinary,
	critical,
	beforePLTE,
	beforeIDAT,
	anyPlace,

	chunkConsNames,
	chunkNamePairs,

	needs
) where

import Prelude hiding (concat)
import Data.ByteString.Lazy (ByteString, concat)
import Data.List (find)
import Data.Char
import Control.Arrow

import Language.Haskell.TH
import Language.Haskell.TH.Tools
import File.Binary.PNG.Chunks.Structures
import File.Binary.PNG.Chunks.Templates
import File.Binary

--------------------------------------------------------------------------------

chunkNames :: [String]
chunkNames = critical ++ beforePLTE ++ beforeIDAT ++ anyPlace

makeDataChunk $ critical ++ beforePLTE ++ beforeIDAT ++ anyPlace

typer ''Chunk 'Others "Chunk"

nameToType $ critical ++ beforePLTE ++ beforeIDAT ++ anyPlace

typeToName $ critical ++ beforePLTE ++ beforeIDAT ++ anyPlace

needs, beforePLTEs, beforeIDATs, anyplaces :: [TypeChunk]
needs = map getType critical
beforePLTEs = map getType beforePLTE
beforeIDATs = map getType beforeIDAT
anyplaces = map getType anyPlace

chunkConsNames :: [Name]
chunkConsNames = map (mkName . ("Chunk" ++) . map toUpper) chunkNames

chunkNamePairs :: [(String, Name)]
chunkNamePairs = zip chunkNames chunkConsNames

mkToBinary :: Name -> ClauseQ
mkToBinary n = clause
	[tupP [varP $ mkName "n", wildP], conP n [varP $ mkName "c"]]
	(normalB $ appsE [varE 'toBinary, varE $ mkName "n", varE $ mkName "c"])
	[]

mkFromBinary :: String -> Name -> ClauseQ
mkFromBinary ns n =
	    clause
		[tupP [varP $ mkName "n", litP $ stringL ns]]
		(normalB $ infixApp
			(varE 'fmap `appE` (varE 'first `appE` conE n))
			(varE '(.))
			(varE 'fromBinary `appE` varE (mkName "n")))
		[]

ihdr :: [Chunk] -> IHDR
ihdr = (\(ChunkIHDR i) -> i) . head . filter ((== T_IHDR) . typeChunk)

plte :: [Chunk] -> Maybe PLTE
plte c = do
	ChunkPLTE pl <- find ((== T_PLTE) . typeChunk) c
	return pl

idats :: [Chunk] -> ByteString
idats = concatIDATs . body'

bplte :: [Chunk] -> [Chunk]
bplte = filter ((`elem` beforePLTEs) . typeChunk)

bidat :: [Chunk] -> [Chunk]
bidat = filter ((`elem` beforeIDATs) . typeChunk)

body' :: [Chunk] -> [IDAT]
body' cs = map (\(ChunkIDAT i) -> i) $ filter ((== T_IDAT) . typeChunk) cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat_body

aplace :: [Chunk] -> [Chunk]
aplace = filter ((`elem` anyplaces) . typeChunk)

others :: [Chunk] -> [Chunk]
others = filter $
	(`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . typeChunk

name :: Chunk -> ByteString
name = getName . typeChunk
