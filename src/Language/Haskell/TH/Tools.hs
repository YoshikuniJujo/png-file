{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Tools (typer) where

import Language.Haskell.TH (
	Name, Info(TyConI), DecsQ, Dec(DataD), Con(NormalC),
	mkName, newName, nameBase, reify,
	cxt, sigD, dataD, normalC, funD, clause, normalB,
	conT, appT, arrowT, conP, varP, wildP, conE, varE, appE)
import Data.List (isPrefixOf)
import Control.Monad (replicateM)
import Control.Arrow (first, (&&&))

--------------------------------------------------------------------------------

typer :: Name -> String -> DecsQ
typer dat prefix = do
	TyConI (DataD _ _ _ s _) <- reify dat
	let	[datN, funN] = map (mkName . (++ nameBase dat)) ["Type", "type"]
		((ns, tns), as) = first (id &&& map (typeName prefix)) $ unzip $
			map (\(NormalC n a) -> (n, map return $ init a)) s
		mkClause n a tn = do
			t <- replicateM (length a) (newName "typ")
			flip (clause [conP n (map varP t ++ [wildP])]) [] $
				normalB $ foldl (\c -> appE c . varE) (conE tn) t
	dd <- dataD (cxt []) datN [] (zipWith normalC tns as) [''Eq, ''Show]
	sd <- sigD funN $ arrowT `appT` conT dat `appT` conT datN
	fd <- funD funN $ zipWith3 mkClause ns as tns
	return [dd, sd, fd]

typeName :: String -> Name -> Name
typeName prefix = mkName . ("T_" ++) . removePrefix prefix . nameBase

removePrefix :: String -> String -> String
removePrefix prefix str
	| prefix `isPrefixOf` str = drop (length prefix) str
	| otherwise = str
