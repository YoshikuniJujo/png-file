{-# LANGUAGE TemplateHaskell #-}

module File.Binary.PNG.Chunks.TH (instanceFieldChunk) where

import Language.Haskell.TH {- (
	Name, mkName, stringL,
	DecsQ, dataD, cxt, normalC, notStrict, strictType, sigD,
	funD, clause, normalB, TypeQ, conT, appT, arrowT, conP, varP, litP,
	conE, varE, appE, litE) -}
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.ByteString.Lazy.Char8 (ByteString)
import File.Binary
import Data.List

--------------------------------------------------------------------------------

chunkConstructors :: [String] -> [Name]
chunkConstructors = map (mkName . ("Chunk" ++))

instanceFieldChunk :: Name -> String -> Name -> DecsQ
instanceFieldChunk tname pre other =
	(:[]) <$> instanceD (cxt []) (conT ''Field `appT` conT tname) [
		tySynInstD ''FieldArgument [conT tname] [t| (Int, ByteString) |],
		funFromBinary 'fromBinary tname pre other,
		funToBinary 'toBinary tname pre other]

funFromBinary :: Name -> Name -> String -> Name -> DecQ
funFromBinary fname tname pre other = do
	TyConI (DataD _ _ _ s _) <- reify tname
	typ <- newName "typ"
	let	cons = filter (/= other) $ map (\(NormalC n _) -> n) s
		chunkNames = map (removePrefix pre . nameBase) cons
	funD fname $ zipWith (mkFromBinary fname)
		(map (litP . stringL) chunkNames ++ [varP typ])
		(map conE (chunkConstructors chunkNames) ++
			[conE other `appE` varE typ])

mkFromBinary :: Name -> PatQ -> ExpQ -> ClauseQ
mkFromBinary fname typ con = do
	n <- newName "n"
	clause [tupP [varP n, typ]]
		(normalB $ infixApp
			(varE 'fmap `appE` (varE 'first `appE` con))
			(varE '(.))
			(varE fname `appE` varE n))
		[]

funToBinary :: Name -> Name -> String -> Name -> DecQ
funToBinary fname tname pre other = do
	TyConI (DataD _ _ _ s _) <- reify tname
	let	cons = filter (/= other) $ map (\(NormalC n _) -> n) s
		chunkNames = map (removePrefix pre . nameBase) cons
	funD fname $ zipWith (mkToBinary fname)
		(chunkConstructors chunkNames ++ [other])
		(replicate (length $ chunkConstructors chunkNames) [] ++ [[wildP]])

mkToBinary :: Name -> Name -> [PatQ] -> ClauseQ
mkToBinary fname con w = do
	[n, dat] <- mapM newName ["n", "dat"]
	clause [tupP [varP n, wildP], conP con $ w ++ [varP dat]]
		(normalB $ appsE [varE fname, varE n, varE dat]) []

removePrefix :: String -> String -> String
removePrefix prefix str
	| prefix `isPrefixOf` str = drop (length prefix) str
	| otherwise = str
