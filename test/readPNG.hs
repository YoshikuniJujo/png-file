{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary.PNG

import Prelude hiding (concat)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin, fout] <- getArgs
	p <- readPNG fin
	let c = chunks p
	putStrLn $ take 1000 (show p) ++ "..."

	let new = png (ihdr p) (plte p) (otherChunks p) (body p)

	writePNG fout new
	putStrLn ""
	putStrLn $ take 1000 (show new) ++ "..."

	BSLC.putStrLn $ BSL.intercalate " " $ map chunkName $ others c

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
