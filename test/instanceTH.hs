{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Control.Applicative
import Control.Monad

some = instanceD (cxt []) (conT ''Show) [other]

other = funD 'show [hoge]

hoge = do
	o <- newName "o"
	clause [conP 'Int [varP o]] (normalB $ varE 'show `appE` varE o) []

data Object
	= Int Int
	| Integer Integer
	| String String
	| Bool Bool

str = "Object"

do	TyConI (DataD [] name [] cons []) <- reify ''Object
	let names = map (\(NormalC n _) -> n) cons
	runIO $ do
		print (name, cons)
		print names
	(: []) <$> instanceD (cxt []) (conT ''Show `appT` conT ''Object) [
		funD 'show $ flip map names $ \n ->
			do	o <- newName "o"
				clause [conP n [varP o]]
					(normalB $ varE 'show `appE` varE o) []
--						varE $ litE $ stringL str) []
	 ]

{-
instance Show Object where
	show (Int o) = show o
	show (Integer o) = show o
	show (String o) = show o
	show (Bool o) = show o
-}
