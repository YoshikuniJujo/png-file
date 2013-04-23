{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary.PNG

import Prelude hiding (concat)
import System.Environment (getArgs)
import File.Binary(readBinaryFile, writeBinaryFile)
import Data.List
import qualified Data.ByteString.Lazy as BSL

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin, fout] <- getArgs
	cnt <- readBinaryFile fin

	let Right cs = getChunks cnt
	putStrLn $ take 700 (show cs) ++ "..."

	let	i = ihdr cs
		p = plte cs
		o = others cs
		b = body cs
		trns = fmap (\(ChunktRNS t) -> t) $ find ((== T_tRNS) . typeChunk) o
		iccp = fmap (\(ChunkiCCP i) -> i) $ find ((== T_iCCP) . typeChunk) o
		ph = makePNGHeader i p trns
		pi :: PNGImageL
		Right pi = bsToPNGImage i p trns b

		(i', Nothing, b') = pngImageToBS pi
		iccp' = fmap (readIccp . iccp_body) iccp
	print ph
	putStrLn $ take 700 (show iccp) ++ "..."
	maybe (return ()) (BSL.writeFile "iccp") iccp'
	putStrLn $ take 700 (show pi) ++ "..."

--	let	binary = putChunks $ mkChunks i p o b
	let	binary' = putChunks $ mkChunks i' p o b'
	writeBinaryFile fout binary'
