{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.Chunks.Chunks (
	Chunk(..),
	TypeChunk(..),
	typeChunk,
	getName,

	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS,
	CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..),
	ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT,
	TIME,

	mkToBinary,
	mkFromBinary,
	critical,
	beforePLTE,
	beforeIDAT,
	anyPlace,

	chunkConsNames,
	chunkNamePairs
) where

import Data.Char
import Control.Arrow

import Language.Haskell.TH
import Language.Haskell.TH.Tools
import File.Binary.PNG.Chunks.Structures hiding
	(critical, beforePLTE, beforeIDAT, anyPlace)
import qualified File.Binary.PNG.Chunks.Structures as S
import File.Binary.PNG.Chunks.Templates
import File.Binary

--------------------------------------------------------------------------------

chunkNames :: [String]
chunkNames = S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

makeDataChunk $ S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

typer ''Chunk 'Others "Chunk"

nameToType $ S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

typeToName $ S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

critical, beforePLTE, beforeIDAT, anyPlace :: [TypeChunk]
critical = map getType S.critical
beforePLTE = map getType S.beforePLTE
beforeIDAT = map getType S.beforeIDAT
anyPlace = map getType S.anyPlace

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
