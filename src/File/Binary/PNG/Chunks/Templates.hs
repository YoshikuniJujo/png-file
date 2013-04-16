{-# LANGUAGE TemplateHaskell #-}

module File.Binary.PNG.Chunks.Templates where

import Language.Haskell.TH
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)

dataChunk :: [String] -> DecsQ
dataChunk chunkNames = do
	let	pairs = map ((mkName . ("Chunk" ++) &&& conT . mkName . id) .
				map toUpper)
			chunkNames
		cons = map (\(c, t) -> normalC c [strictType (return NotStrict) t])
			pairs
		bs = strictType (return NotStrict) $ conT ''ByteString
		others = normalC (mkName "Others") [bs, bs]
	dd <- dataD (cxt []) (mkName "Chunk") [] (cons ++ [others]) [''Show]
	return [dd]

typeNameTable :: [String] -> DecsQ
typeNameTable chunkNames = do
	let	tups = map
			((\(t, n) -> tupE [t, n]) .
				(conE . mkName . ("T_" ++) . map toUpper &&&
					litE . stringL))
			chunkNames
	sd <- sigD (mkName "typeNameT") $ listT `appT`
		(tupleT 2 `appT` conT (mkName "TypeChunk") `appT` conT ''ByteString)
	vd <- valD (varP $ mkName "typeNameT") (normalB $ listE tups) []
	return [sd, vd]

nameType :: [String] -> DecsQ
nameType chunkNames = (++) <$> nameToType chunkNames <*> typeToName chunkNames

nameToType :: [String] -> DecsQ
nameToType chunkNames = do
	let	pair = map (id &&& conE . mkName . ("T_" ++) . map toUpper) chunkNames
		mkClause arg ret =
			clause [litP $ stringL arg] (normalB ret) []
		otherClause = flip (clause [varP $ mkName "str"]) [] $ normalB $
			conE (mkName "T_Others") `appE`
				(varE 'pack `appE` varE (mkName "str"))
	sd <- sigD (mkName "nameToType") $
		arrowT `appT` conT ''String `appT` conT (mkName "TypeChunk")
	fd <- funD (mkName "nameToType") $ map (uncurry mkClause) pair ++ [otherClause]
	return [sd, fd]

typeToName :: [String] -> DecsQ
typeToName chunkNames = do
	let	pair = map (mkName . ("T_" ++) . map toUpper &&& id) chunkNames
		mkClause arg ret =
			clause [conP arg []] (normalB $ litE $ stringL ret) []
		otherClause = clause
			[conP (mkName "T_Others") [varP $ mkName "str"]]
			(normalB $ varE $ mkName "str") []
	sd <- sigD (mkName "typeToName") $
		arrowT `appT` conT (mkName "TypeChunk") `appT` conT ''ByteString
	fd <- funD (mkName "typeToName") $ map (uncurry mkClause) pair ++ [otherClause]
	return [sd, fd]
