{-# LANGUAGE TemplateHaskell #-}

module File.Binary.PNG.Chunks.Templates (dataChunk, typeName) where

import Language.Haskell.TH (
	Name, mkName, stringL,
	DecsQ, dataD, cxt, normalC, notStrict, strictType, sigD,
	funD, clause, normalB, TypeQ, conT, appT, arrowT, conP, varP, litP,
	conE, varE, appE, litE)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Data.Char (toUpper)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

--------------------------------------------------------------------------------

dataChunk :: [String] -> DecsQ
dataChunk chunks = do
	let	con = flip map chunks $ (.)
			(normalC <$> fst <*> (: []) . strictType notStrict . snd)
			((mkName . ("Chunk" ++) &&& conT . mkName . id) . upper)
		bs = strictType notStrict $ conT ''ByteString
		others = normalC (mkName "Others") [bs, bs]
	(: []) <$> dataD (cxt []) (mkName "Chunk") [] (con ++ [others]) [''Show]

typeName :: [String] -> DecsQ
typeName chunks = let types = map (mkName . ("T_" ++) . upper) chunks in
	(++) <$> nameToType chunks types <*> typeToName types chunks

nameToType :: [String] -> [Name] -> DecsQ
nameToType chunks types = do
	let	mkClause n t = clause [litP $ stringL n] (normalB $ conE t) []
		other = flip (clause [varP $ mkName "str"]) [] $ normalB $
			conE tothers `appE` (varE 'pack `appE` varE (mkName "str"))
	sd <- sigD ntt $ conT ''String --> conT tchunk
	fd <- funD ntt $ zipWith mkClause chunks types ++ [other]
	return [sd, fd]

typeToName :: [Name] -> [String] -> DecsQ
typeToName types chunks = do
	let	mkClause t n = clause [conP t []] (normalB $ litE $ stringL n) []
		other = clause [conP tothers [varP $ mkName "str"]]
			(normalB $ varE $ mkName "str") []
	sd <- sigD ttn $ conT tchunk --> conT '' ByteString
	fd <- funD ttn $ zipWith mkClause types chunks ++ [other]
	return [sd, fd]

ntt, ttn, tchunk, tothers :: Name
[ntt, ttn, tchunk, tothers] =
	map mkName ["nameToType", "typeToName", "TypeChunk", "T_Others"]

upper :: String -> String
upper = map toUpper

(-->) :: TypeQ -> TypeQ -> TypeQ
t1 --> t2 = arrowT `appT` t1 `appT` t2
