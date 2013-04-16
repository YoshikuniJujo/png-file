{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes,
	TypeFamilies,
	FlexibleInstances,
	OverloadedStrings #-}
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

	instanceFieldChunk, fieldArgument, funFromBinary, funToBinary,

	beforePLTE,
	beforeIDAT,
	anyPlace,
) where

import Data.Char
import Control.Arrow
import Control.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Tools
import File.Binary.PNG.Chunks.Structures hiding
	(critical, beforePLTE, beforeIDAT, anyPlace)
import qualified File.Binary.PNG.Chunks.Structures as S
import File.Binary.PNG.Chunks.Templates
import File.Binary
import Data.ByteString.Lazy (ByteString)

--------------------------------------------------------------------------------

chunkNames :: [String]
chunkNames = S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

makeDataChunk $ S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

typer ''Chunk 'Others "Chunk"

nameToType $ S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

typeToName $ S.critical ++ S.beforePLTE ++ S.beforeIDAT ++ S.anyPlace

beforePLTE, beforeIDAT, anyPlace :: [TypeChunk]
beforePLTE = map getType S.beforePLTE
beforeIDAT = map getType S.beforeIDAT
anyPlace = map getType S.anyPlace

chunkConsNames :: [Name]
chunkConsNames = map (mkName . ("Chunk" ++) . map toUpper) chunkNames

chunkNamePairs :: [(String, Name)]
chunkNamePairs = zip chunkNames chunkConsNames

funToBinary :: DecQ
funToBinary = funD 'toBinary $ map mkToBinary chunkConsNames ++ [mkToBinaryOthers]

mkToBinary :: Name -> ClauseQ
mkToBinary n = clause
	[tupP [varP $ mkName "n", wildP], conP n [varP $ mkName "c"]]
	(normalB $ appsE [varE 'toBinary, varE $ mkName "n", varE $ mkName "c"])
	[]

mkToBinaryOthers :: ClauseQ
mkToBinaryOthers = clause
	[tupP [varP $ mkName "n", wildP], conP 'Others [wildP, varP $ mkName "str"]]
	(normalB $ appsE [varE 'toBinary, varE $ mkName "n", varE $ mkName "str"])
	[]

instanceFieldChunk :: DecsQ
instanceFieldChunk =
	(:[]) <$> instanceD (cxt []) (conT ''Field `appT` conT ''Chunk)
		[fieldArgument, funFromBinary, funToBinary]

fieldArgument :: DecQ
fieldArgument = tySynInstD ''FieldArgument [conT ''Chunk]
	(tupleT 2 `appT` conT ''Int `appT` conT ''ByteString)

funFromBinary :: DecQ
funFromBinary = funD 'fromBinary $ map (uncurry mkFromBinary) chunkNamePairs ++
	[mkFromBinaryOthers]

mkFromBinary :: String -> Name -> ClauseQ
mkFromBinary ns n = clause
	[tupP [varP $ mkName "n", litP $ stringL ns]]
	(normalB $ infixApp
		(varE 'fmap `appE` (varE 'first `appE` conE n))
		(varE '(.))
		(varE 'fromBinary `appE` varE (mkName "n")))
	[]

mkFromBinaryOthers :: ClauseQ
mkFromBinaryOthers = clause
	[tupP [varP $ mkName "n", varP $ mkName "nam"]]
	(normalB $ infixApp
		(varE 'fmap `appE` (varE 'first `appE`
			(conE 'Others `appE` varE (mkName "nam"))))
		(varE '(.))
		(varE 'fromBinary `appE` varE (mkName "n")))
	[]
