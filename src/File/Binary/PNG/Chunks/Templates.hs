{-# LANGUAGE TemplateHaskell #-}

module File.Binary.PNG.Chunks.Templates where

import Language.Haskell.TH
import Control.Arrow
import Data.Char
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)

makeDataChunk :: [String] -> DecsQ
makeDataChunk chunkNames = do
	let	pairs = map ((mkName . ("Chunk" ++) &&& conT . mkName . id) .
				map toUpper)
			chunkNames
		cons = map (\(c, t) -> normalC c [strictType (return NotStrict) t])
			pairs
		bs = strictType (return NotStrict) $ conT ''ByteString
		others = normalC (mkName "Others") [bs, bs]
	dd <- dataD (cxt []) (mkName "Chunk") [] (cons ++ [others]) [''Show]
	return [dd]

nameToType :: [String] -> DecsQ
nameToType chunkNames = do
	let	pair = map (id &&& conE . mkName . ("T_" ++) . map toUpper) chunkNames
		mkClause arg ret =
			clause [litP $ stringL arg] (normalB ret) []
		otherClause = flip (clause [varP $ mkName "str"]) [] $ normalB $
			conE (mkName "T_Others") `appE`
				(varE 'pack `appE` varE (mkName "str"))
	sd <- sigD (mkName "getType") $
		arrowT `appT` conT ''String `appT` conT (mkName "TypeChunk")
	fd <- funD (mkName "getType") $ map (uncurry mkClause) pair ++ [otherClause]
	return [sd, fd]

typeToName :: [String] -> DecsQ
typeToName chunkNames = do
	let	pair = map (mkName . ("T_" ++) . map toUpper &&& id) chunkNames
		mkClause arg ret =
			clause [conP arg []] (normalB $ litE $ stringL ret) []
		otherClause = clause
			[conP (mkName "T_Others") [varP $ mkName "str"]]
			(normalB $ varE $ mkName "str") []
	sd <- sigD (mkName "getName") $
		arrowT `appT` conT (mkName "TypeChunk") `appT` conT ''ByteString
	fd <- funD (mkName "getName") $ map (uncurry mkClause) pair ++ [otherClause]
	return [sd, fd]
