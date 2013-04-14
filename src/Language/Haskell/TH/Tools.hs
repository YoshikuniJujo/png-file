{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Tools (
	typer
) where

import Language.Haskell.TH
import Control.Arrow
import Data.List
import Data.Maybe

{-
data Select = All | Only [Name] | Hiding [Name] deriving Show
data AddType = AddType [String]

typer' :: Name -> Select -> AddType -> DecsQ
typer' con (Hiding hs) (AddType ats) = do
	TyConI (DataD _ _ _ s _) <- reify con
	let	ns = map (\(NormalC n _) -> n) s
		names = map (mkName . ("T_" ++)) $ (++ ats) $ map nameBase $
			filter (`notElem` hs) ns
		pairs = map (flip recP [] . mkName &&& conE . mkName . ("T_" ++)) $
			map nameBase $ filter (`notElem` hs) ns
		cons = map (return . flip NormalC []) names
	runIO $ print names
	let tcon = mkName $ "Type" ++ nameBase con
	dd <- dataD (cxt []) tcon [] cons [''Eq, ''Show]
	fd <- funD (mkName $ "type" ++ nameBase con) $ flip map pairs $ \(p, e) ->
		clause [p] (normalB e) []
	return [dd, fd]
-}

typer :: Name -> Name -> String -> DecsQ
typer con ot remove = do
	TyConI (DataD _ _ _ s _) <- reify con
	ovar <- newName "ovar"
	let	ns = map (\(NormalC n _) -> n) s
		other = fromJust $ find (\(NormalC n _) -> n == ot) s
		names = map (mkName . mkT_con remove) $ map nameBase $ filter (/= ot) ns
		pairs = map (flip recP [] . mkName &&& conE . mkName . mkT_con remove) $
			map nameBase $ filter (/= ot) ns
		otherN = (\(NormalC on _) -> mkName $ mkT_con remove $ nameBase on) other
		otherT = (\(NormalC on oa) -> NormalC
			(mkName $ mkT_con remove $ nameBase on) [head oa]) other
		otherP = (\(NormalC on _) -> conP on [varP ovar, wildP]) other
		cons = map (return . flip NormalC []) names ++ [return otherT]
		otherClause = clause [otherP]
			(normalB $ conE otherN `appE` varE ovar) []
{-
	runIO $ do
		print names
		print other
		print otherT
-}
	let tcon = mkName $ "Type" ++ nameBase con
	sd <- sigD (mkName $ "type" ++ nameBase con) $
		arrowT `appT` conT con `appT` conT (mkName $ "Type" ++ nameBase con)
	dd <- dataD (cxt []) tcon [] cons [''Eq, ''Show]
	fd <- funD (mkName $ "type" ++ nameBase con) $ (flip map pairs $ \(p, e) ->
		clause [p] (normalB e) []) ++ [otherClause]
	return [sd, dd, fd]

mkT_con :: String -> String -> String
mkT_con remove = ("T_" ++) . removeStr remove

removeStr :: String -> String -> String
removeStr remove str
	| remove `isPrefixOf` str = drop (length remove) str
	| otherwise = str
