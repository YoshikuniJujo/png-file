{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Tools (wrapTypes, typeName, typer) where

import Language.Haskell.TH {- (
	Name, Info(TyConI), DecsQ, Dec(DataD), Con(NormalC),
	mkName, newName, nameBase, reify,
	cxt, sigD, dataD, normalC, funD, clause, normalB,
	conT, appT, arrowT, conP, varP, wildP, conE, varE, appE,
	strictType, notStrict) -}
import Data.List (isPrefixOf)
import Control.Monad (replicateM)
import Control.Arrow (first, (&&&))
import Control.Applicative
import Data.Char
import Data.String

--------------------------------------------------------------------------------

wrapTypes :: String -> [String] -> (String, [Name]) -> [Name] -> DecsQ
wrapTypes name types other deriv =
	fmap (: []) $ flip (dataD (cxt []) (mkName name) []) deriv $ map
		(normalC <$> fst <*> map (strictType notStrict . conT) . snd) $
			(++ [first mkName other]) $ flip map types $ (. upper) $
				mkName . (name ++) &&& (: []) . mkName

typer :: Name -> String -> String -> DecsQ
typer dat preold  prenew = do
	TyConI (DataD _ _ _ s _) <- reify dat
	let	[datN, funN] = map (mkName . (++ nameBase dat)) ["Type", "type"]
		((ns, tns), as) = first (id &&& map (typeName' preold prenew)) $
			unzip $ map (\(NormalC n a) -> (n, map return $ init a)) s
		mkClause n a tn = do
			t <- replicateM (length a) (newName "typ")
			flip (clause [conP n (map varP t ++ [wildP])]) [] $
				normalB $ foldl (\c -> appE c . varE) (conE tn) t
	dd <- dataD (cxt []) datN [] (zipWith normalC tns as) [''Eq, ''Show]
	sd <- sigD funN $ arrowT `appT` conT dat `appT` conT datN
	fd <- funD funN $ zipWith3 mkClause ns as tns
	return [dd, sd, fd]

typeName' :: String -> String -> Name -> Name
typeName' preold prenew = mkName . (prenew ++) . removePrefix preold . nameBase

removePrefix :: String -> String -> String
removePrefix prefix str
	| prefix `isPrefixOf` str = drop (length prefix) str
	| otherwise = str

typeName :: Name -> [String] -> String -> (Name, Name) -> DecsQ
typeName typ chunks pre to = let types = map (mkName . (pre ++) . upper) chunks in
	(++) <$> nameToType typ chunks types (fst to) <*> typeToName typ types chunks to

nameToType :: Name -> [String] -> [Name] -> Name -> DecsQ
nameToType typ chunks types tothers = do
	let	mkClause n t = clause [litP $ stringL n] (normalB $ conE t) []
		other = flip (clause [varP $ mkName "str"]) [] $ normalB $
			conE tothers `appE` (varE 'fromString `appE` varE (mkName "str"))
	sd <- sigD (ntt typ) $ conT ''String --> conT typ
	fd <- funD (ntt typ) $ zipWith mkClause chunks types ++ [other]
	return [sd, fd]

typeToName :: Name -> [Name] -> [String] -> (Name, Name) -> DecsQ
typeToName typ types chunks (tothers, totype) = do
	let	mkClause t n = clause [conP t []] (normalB $ litE $ stringL n) []
		other = clause [conP tothers [varP $ mkName "str"]]
			(normalB $ varE $ mkName "str") []
	sd <- sigD (ttn typ) $ conT typ --> conT totype
	fd <- funD (ttn typ) $ zipWith mkClause types chunks ++ [other]
	return [sd, fd]

ntt, ttn :: Name -> Name
ntt = mkName . ("nameTo" ++) . nameBase
ttn = mkName . (++ "ToName") . headToLower . nameBase

headToLower :: String -> String
headToLower "" = ""
headToLower (c : cs) = toLower c : cs

upper :: String -> String
upper = map toUpper

(-->) :: TypeQ -> TypeQ -> TypeQ
t1 --> t2 = arrowT `appT` t1 `appT` t2
