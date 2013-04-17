{-# LANGUAGE TemplateHaskell #-}

module File.Binary.PNG.Chunks.TH (instanceFieldChunk) where

import Language.Haskell.TH {- (
	Name, mkName, stringL,
	DecsQ, dataD, cxt, normalC, notStrict, strictType, sigD,
	funD, clause, normalB, TypeQ, conT, appT, arrowT, conP, varP, litP,
	conE, varE, appE, litE) -}
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Char (toUpper)
import Data.ByteString.Lazy.Char8 (ByteString)
import File.Binary

--------------------------------------------------------------------------------

chunkConstructors :: [String] -> [Name]
chunkConstructors = map (mkName . ("Chunk" ++) . map toUpper)

instanceFieldChunk :: [String] -> DecsQ
instanceFieldChunk chunkNames =
	(:[]) <$> instanceD (cxt []) (conT ''Field `appT` conT (mkName "Chunk"))
		[fieldArgument, funFromBinary chunkNames, funToBinary chunkNames]

fieldArgument :: DecQ
fieldArgument =
	tySynInstD ''FieldArgument [conT $ mkName "Chunk"] $ [t| (Int, ByteString) |]

funFromBinary :: [String] -> DecQ
funFromBinary chunkNames = funD 'fromBinary $ zipWith mkFromBinary
	(map (litP . stringL) chunkNames ++ [varP $ mkName "name"])
	(map conE (chunkConstructors chunkNames) ++
		[conE (mkName "Others") `appE` varE (mkName "name")])

mkFromBinary :: PatQ -> ExpQ -> ClauseQ
mkFromBinary name con = clause [tupP [varP $ mkName "n", name]]
	(normalB $ infixApp
		(varE 'fmap `appE` (varE 'first `appE` con))
		(varE '(.))
		(varE 'fromBinary `appE` varE (mkName "n")))
	[]

funToBinary :: [String] -> DecQ
funToBinary chunkNames = funD 'toBinary $ zipWith mkToBinary
	(chunkConstructors chunkNames ++ [mkName "Others"])
	(replicate (length $ chunkConstructors chunkNames) [] ++ [[wildP]])

mkToBinary :: Name -> [PatQ] -> ClauseQ
mkToBinary con w = clause
	[tupP [varP $ mkName "n", wildP], conP con $ w ++ [varP $ mkName "c"]]
	(normalB $ appsE [varE 'toBinary, varE $ mkName "n", varE $ mkName "c"])
	[]
