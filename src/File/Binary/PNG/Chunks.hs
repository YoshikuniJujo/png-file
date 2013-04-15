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

	mkToBinary,
	mkFromBinary,
	critical,
	beforePLTE,
	beforeIDAT,
	anyPlace,

	chunkConsNames,
	chunkNamePairs,

	needs,
	beforePLTEs,
	beforeIDATs,
	anyplaces
) where

import Data.ByteString.Lazy (ByteString)
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

name :: Chunk -> ByteString
name = getName . typeChunk
