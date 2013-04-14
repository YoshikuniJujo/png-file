{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Tools
import Language.Haskell.TH

data Hoge
	= Are Int
	| Ha Int
	| Ittai Int
	| Nan Int
	| Nano Int
	| Da String Int
	deriving Show

-- typer ''Hoge (Hiding ['Da]) (AddType ["Da1", "Da2"])
typer ''Hoge 'Da ""

main :: IO ()
main = do
	print $ Nan 8
	print $ T_Are
	print $ typeHoge $ Nan 8
	print $ typeHoge $ Da "hage" 9
