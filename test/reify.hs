{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data Some
	= Some Int
	| Other String Int

do	info <- reify ''Some
	runIO $ print info
	return []

main = print "hoge"
