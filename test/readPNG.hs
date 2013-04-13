{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary.PNG

import Prelude hiding (concat)
import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin, fout] <- getArgs
	cnt <- readBinaryFile fin

	let Right chs = readPNG cnt
	putStrLn $ take 700 (show chs) ++ "..."

	let	i = ihdr chs
		p = plte chs
		o = otherChunks chs
		b = body chs
		new = png i p o b

	let binary = writePNG new
	writeBinaryFile fout binary

	putStrLn ""
	putStrLn $ take 700 (show new) ++ "..."

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
