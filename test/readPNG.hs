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
	putStrLn $ take 1000 (show chs) ++ "..."

	let new = png (ihdr chs) (plte chs) (otherChunks chs) (body chs)

	writePNG fout new
	putStrLn ""
	putStrLn $ take 1000 (show new) ++ "..."

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
