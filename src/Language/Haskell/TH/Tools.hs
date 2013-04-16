{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Tools (typer) where

import Language.Haskell.TH
import Control.Arrow
import Data.List
import Data.Maybe

typer :: Name -> Name -> String -> DecsQ
typer con ot remove = do
	TyConI (DataD _ _ _ s _) <- reify con
	ovar <- newName "ovar"
	let	ns = map (\(NormalC n _) -> n) s
		other = fromJust $ find (\(NormalC n _) -> n == ot) s
		names = map (mkName . mkTCon remove . nameBase) $
			filter (/= ot) ns
		pairs = map ((flip recP [] . mkName &&& conE . mkName .
				mkTCon remove) . nameBase) $ filter (/= ot) ns
		otherN = (\(NormalC on _) -> mkName $
			mkTCon remove $ nameBase on) other
		otherT = (\(NormalC on oa) -> NormalC
			(mkName $ mkTCon remove $ nameBase on) [head oa]) other
		otherP = (\(NormalC on _) -> conP on [varP ovar, wildP]) other
		cons = map (return . flip NormalC []) names ++ [return otherT]
		otherClause = clause [otherP]
			(normalB $ conE otherN `appE` varE ovar) []
	let tcon = mkName $ "Type" ++ nameBase con
	sd <- sigD (mkName $ "type" ++ nameBase con) $
		arrowT `appT` conT con `appT` conT (mkName $ "Type" ++ nameBase con)
	dd <- dataD (cxt []) tcon [] cons [''Eq, ''Show]
	fd <- funD (mkName $ "type" ++ nameBase con) $
		map ( \(p, e) -> clause [p] (normalB e) []) pairs ++
			[otherClause]
	return [sd, dd, fd]

mkTCon :: String -> String -> String
mkTCon remove = ("T_" ++) . removeStr remove

removeStr :: String -> String -> String
removeStr remove str
	| remove `isPrefixOf` str = drop (length remove) str
	| otherwise = str
