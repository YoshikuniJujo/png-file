{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.Chunks.Chunks (
	Chunk(..), TypeChunk(..), typeChunk, typeToName,
	instanceFieldChunk,

	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS, CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..), ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME,
	bplte, bidat, aplace,
) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Char (toUpper)
import Data.ByteString.Lazy (ByteString)
import Language.Haskell.TH (
	Name, mkName, stringL,
	DecQ, DecsQ, instanceD, tySynInstD, cxt, funD, ClauseQ, clause, normalB,
	conT, appT, PatQ, conP, varP, litP, tupP, wildP,
	ExpQ, conE, varE, appE, appsE, infixApp)
import Language.Haskell.TH.Tools (typer)
import File.Binary (Field(..))
import File.Binary.PNG.Chunks.Each (
	IHDR(..), PLTE(..), IDAT(..), IEND(..),
	TRNS, CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..), ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME,
	chunkNames, beforePLTE, beforeIDAT, anyPlace)
import File.Binary.PNG.Chunks.Templates (dataChunk, typeName)

--------------------------------------------------------------------------------

dataChunk chunkNames
typer ''Chunk 'Others "Chunk"
typeName chunkNames

bplte, bidat, aplace :: [TypeChunk]
[bplte, bidat, aplace] = map (map nameToType) [beforePLTE, beforeIDAT, anyPlace]

chunkConstructors :: [Name]
chunkConstructors = map (mkName . ("Chunk" ++) . map toUpper) chunkNames

instanceFieldChunk :: DecsQ
instanceFieldChunk = (:[]) <$> instanceD (cxt []) (conT ''Field `appT` conT ''Chunk)
	[fieldArgument, funFromBinary, funToBinary]

fieldArgument :: DecQ
fieldArgument = tySynInstD ''FieldArgument [conT ''Chunk] $ [t| (Int, ByteString) |]

funFromBinary :: DecQ
funFromBinary = funD 'fromBinary $ zipWith mkFromBinary
	(map (litP . stringL) chunkNames ++ [varP $ mkName "name"])
	(map conE chunkConstructors ++ [conE 'Others `appE` varE (mkName "name")])

mkFromBinary :: PatQ -> ExpQ -> ClauseQ
mkFromBinary name con = clause [tupP [varP $ mkName "n", name]]
	(normalB $ infixApp
		(varE 'fmap `appE` (varE 'first `appE` con))
		(varE '(.))
		(varE 'fromBinary `appE` varE (mkName "n")))
	[]

funToBinary :: DecQ
funToBinary = funD 'toBinary $ zipWith mkToBinary
	(chunkConstructors ++ ['Others])
	(replicate (length chunkConstructors) [] ++ [[wildP]])

mkToBinary :: Name -> [PatQ] -> ClauseQ
mkToBinary con w = clause
	[tupP [varP $ mkName "n", wildP], conP con $ w ++ [varP $ mkName "c"]]
	(normalB $ appsE [varE 'toBinary, varE $ mkName "n", varE $ mkName "c"])
	[]
