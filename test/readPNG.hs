{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary.PNG

import Prelude hiding (concat)
import System.Environment (getArgs)
import File.Binary(readBinaryFile, writeBinaryFile)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin, fout] <- getArgs
	cnt <- readBinaryFile fin

	let Right cs = readPNG cnt
	putStrLn $ take 700 (show cs) ++ "..."

	let	i = ihdr cs
		p = plte cs
		o = others cs
		b = body cs

	let binary = writePNG i p o b
	writeBinaryFile fout binary

{-
ihdr :: IHDR
ihdr = IHDR {
	width = 700,
	height = 700,
	depth = 8,
	alpha = False,
	color = True,
	palet = False,
	compressionType = 0,
	filterType = 0,
	interlaceType = 0 }
-}
