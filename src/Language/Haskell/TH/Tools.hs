{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Tools (
	mapTypesFun,
	wrapTypes,
	makeTypes,
	nameTypes
) where

import Language.Haskell.TH (
	Info(TyConI), reify, Name, mkName, newName, nameBase, stringL,
	DecsQ, DecQ, Dec(FunD, DataD), cxt, sigD, dataD, funD,
	Con(NormalC), normalC, ClauseQ, clause, normalB,
	TypeQ, Type, conT, appT, arrowT, conP, varP, wildP, litP,
	conE, varE, appE, litE, strictType, notStrict)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, (&&&))
import Control.Monad (replicateM)
import Data.List (isPrefixOf)
import Data.String (fromString)
import Data.Char (toLower, toUpper)

--------------------------------------------------------------------------------

mapTypesFun :: Name -> Name -> (Name -> [Type] -> ClauseQ) -> DecQ
mapTypesFun fname typ f = do
	TyConI (DataD _ _ _ cons _) <- reify typ
	clauses <- (`mapM` cons) $ \(NormalC n a) -> f n $ map snd a
	return $ FunD fname clauses

wrapTypes :: String -> [String] -> (String, [Name]) -> [Name] -> DecsQ
wrapTypes name types other deriv = let upper = map toUpper in fmap (: []) $
	flip (dataD (cxt []) (mkName name) []) deriv $ map
		(normalC <$> fst <*> map (strictType notStrict . conT) . snd) $
			(++ [first mkName other]) $ flip map types $
				mkName . (name ++) &&& (: []) . mkName . upper

makeTypes :: String -> Name -> String -> String -> DecsQ
makeTypes name dat preold prenew = do
	TyConI (DataD _ _ _ cs _) <- reify dat
	let	(datN, funN) = mkName &&& mkName . headToLower $ name
		((ns, tns), as) = first (id &&& map chpre) $ unzip $
			map (\(NormalC n a) -> (n, map return $ init a)) cs
		mkClause n a tn = do
			t <- replicateM (length a) (newName "typ")
			flip (clause [conP n (map varP t ++ [wildP])]) [] $
				normalB $ foldl (\c -> appE c . varE) (conE tn) t
	dd <- dataD (cxt []) datN [] (zipWith normalC tns as) [''Eq, ''Show]
	sd <- sigD funN $ conT dat --> conT datN
	fd <- funD funN $ zipWith3 mkClause ns as tns
	return [dd, sd, fd]
	where chpre = mkName . (prenew ++) . removePrefix preold . nameBase

removePrefix :: String -> String -> String
removePrefix pre str
	| pre `isPrefixOf` str = drop (length pre) str
	| otherwise = str

nameTypes :: Name -> String -> Name -> Name -> DecsQ
nameTypes typ pre o st = do
	TyConI (DataD _ _ _ cons _) <- reify typ
	let	types = filter (/= o) $ map (\(NormalC n _) -> n) cons
		cs = map (removePrefix pre . nameBase) types
	(++) <$> nameToType typ cs types o <*> typeToName typ types cs o st

nameToType :: Name -> [String] -> [Name] -> Name -> DecsQ
nameToType typ strs types o = do
	str <- newName "str"
	let	pats = map ((: []) . litP . stringL) strs ++ [[varP str]]
		bodys = map normalB $ map conE types ++
			[conE o `appE` (varE 'fromString `appE` varE str)]
	(\sd fd -> [sd, fd])
		<$> sigD fname (conT ''String --> conT typ)
		<*> funD fname (zipWith3 clause pats bodys $ repeat [])
	where fname = mkName $ ("nameTo" ++) $ nameBase typ

typeToName :: Name -> [Name] -> [String] -> Name -> Name -> DecsQ
typeToName typ ts ss o st = do
	str <- newName "str"
	let	pats = map ((: []) . ($ []) . conP) ts ++ [[conP o [varP str]]]
		bodys = map normalB $ map (litE . stringL) ss ++ [varE str]
	(\sd fd -> [sd, fd])
		<$> sigD fname (conT typ --> conT st)
		<*> funD fname (zipWith3 clause pats bodys $ repeat [])
	where fname = mkName $ (++ "ToName") $ headToLower $ nameBase typ

(-->) :: TypeQ -> TypeQ -> TypeQ
t1 --> t2 = arrowT `appT` t1 `appT` t2

headToLower :: String -> String
headToLower "" = ""
headToLower (c : cs) = toLower c : cs
