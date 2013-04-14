{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.Chunks (
	Chunk(..),
	IHDR(..),
	CHRM(..),
	GAMA(..),
	SRGB(..),
	PLTE(..),
	BKGD(..),
	IDAT(..),
	TEXT(..),
	IEND(..),
	typeChunk,
	name,
	ihdr,
	bplte,
	plte,
	bidat,
	idats,
	aplace,
	others
) where

import Prelude hiding (concat)
import Data.ByteString.Lazy (ByteString, concat)
import Data.List (find)

import Language.Haskell.TH.Tools
import File.Binary.PNG.Chunks.Structures

--------------------------------------------------------------------------------

data Chunk
	= ChunkIHDR IHDR
	| ChunkCHRM CHRM
	| ChunkGAMA GAMA
--	| ChunkSBIT SBIT
	| ChunkSRGB SRGB
--	| ChunkICCP ICCP
	| ChunkPLTE PLTE
	| ChunkBKGD BKGD
--	| ChunkHIST HIST
--	| ChunkTRNS TRNS
--	| ChunkPHYS PHYS
--	| ChunkSPLT SPLT
--	| ChunkOFFS OFFS
--	| ChunkPCAL PCAL
--	| ChunkSCAL SCAL
	| ChunkIDAT IDAT
--	| ChunkTIME TIME
	| ChunkTEXT TEXT
--	| ChunkZTXT ZTXT
--	| ChunkITXT ITXT
--	| ChunkGIFG GIFG
--	| ChunkGIFT GIFT
--	| ChunkGIFX GIFX
--	| ChunkFRAC FRAC
	| ChunkIEND IEND
	| Others ByteString ByteString
	deriving Show

typer ''Chunk 'Others "Chunk"

nameTable :: [(TypeChunk, ByteString)]
nameTable = [
	(T_IHDR, "IHDR"),
	(T_CHRM, "cHRM")
 ]

ihdr :: [Chunk] -> IHDR
ihdr = (\(ChunkIHDR i) -> i) . head . filter ((== T_IHDR) . typeChunk)

bplte :: [Chunk] -> [Chunk]
bplte = filter ((`elem` beforePLTEs) . typeChunk)

plte :: [Chunk] -> Maybe PLTE
plte c = do
	ChunkPLTE pl <- find ((== T_PLTE) . typeChunk) c
	return pl

bidat :: [Chunk] -> [Chunk]
bidat = filter ((`elem` beforeIDATs) . typeChunk)

idats :: [Chunk] -> ByteString
idats = concatIDATs . body'

body' :: [Chunk] -> [IDAT]
body' cs = map (\(ChunkIDAT i) -> i) $ filter ((== T_IDAT) . typeChunk) cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat_body

aplace :: [Chunk] -> [Chunk]
aplace = filter ((`elem` anyplaces) . typeChunk)

others :: [Chunk] -> [Chunk]
others = filter $
	(`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . typeChunk

needs :: [TypeChunk]
needs = [T_IHDR, T_PLTE, T_IDAT, T_IEND]

beforePLTEs :: [TypeChunk]
-- beforePLTEs = [T_CHRM, T_GAMA, T_SBIT, T_SRGB, T_ICCP]
beforePLTEs = [T_CHRM, T_GAMA, T_Others "sBIT", T_SRGB, T_Others "iCCP"]

beforeIDATs :: [TypeChunk]
-- beforeIDATs = [T_BKGD, T_HIST, T_TRNS, T_PHYS, T_SPLT, T_OFFS, T_PCAL, T_SCAL]
beforeIDATs = [T_BKGD, T_Others "hIST", T_Others "tRNS", T_Others "pHYs",
	T_Others "sPLT", T_Others "oFFs", T_Others "pCAL", T_Others "sCAL"]

anyplaces :: [TypeChunk]
-- anyplaces = [T_TIME, T_TEXT, T_ZTXT, T_ITXT, T_GIFG, T_GIFT, T_GIFX, T_FRAC]
-- anyplaces = ["tIME", "tEXt", "zTXt", "iTXt", "gIFg", "gIFt", "gIFx", "fRAc"]
anyplaces = [T_Others "tIME", T_TEXT, T_Others "zTXt", T_Others "iTXt",
	T_Others "gIFg", T_Others "gIFt", T_Others "gIFx", T_Others "fRAc"]

name :: Chunk -> ByteString
name (ChunkIHDR _) = "IHDR"
name (ChunkPLTE _) = "PLTE"
name (ChunkIEND _) = "IEND"
name (ChunkGAMA _) = "gAMA"
name (ChunkSRGB _) = "sRGB"
name (ChunkCHRM _) = "cHRM"
name (ChunkBKGD _) = "bKGD"
name (ChunkTEXT _) = "tEXt"
name (ChunkIDAT _) = "IDAT"
name (Others n _) = n
