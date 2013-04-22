{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary.PNG

import Prelude hiding (concat)
import System.Environment (getArgs)
import File.Binary(readBinaryFile, writeBinaryFile)
import DrawBitmap
import qualified Data.ByteString.Lazy as BSL (length)
import Data.List

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin] <- getArgs
	cnt <- readBinaryFile fin

	let Right cs = getChunks cnt
	putStrLn $ take 700 (show cs) ++ "..."

	let	i = ihdr cs
		w = fromIntegral $ width i
		h = fromIntegral $ height i
		p = plte cs
		o = others cs
		b = body cs

		trns = fmap (\(ChunktRNS t) -> t) $ find ((== T_tRNS) . typeChunk) o
		ph = makePNGHeader i trns
		Right (PNGImageL False [] pi)  = bsToPNGImage i trns b
		fi = fromIntegral
		convert (PNGImageLColor r g b 255) = (fi r, fi g, fi b)
	putStrLn $ take 700 (show b) ++ "..."
--	putStrLn $ (show b)
	print $ BSL.length b
--	drawBitmap w h b
--	drawBitmap2 $ bsToImage w b
	drawBitmap2 $ map ((map convert) . snd) pi

--	let binary = putChunks $ mkChunks i p o b
--	writeBinaryFile fout binary

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
