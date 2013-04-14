{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
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
	chunkType,
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
import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import Data.ByteString.Lazy (ByteString, concat)
import Data.Monoid
import Data.List (find)

import Language.Haskell.TH.Tools

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
	| ChunkIDAT { cidat :: IDAT }
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

data ChunkType
	= T_IHDR
	| T_CHRM | T_GAMA | T_SBIT | T_SRGB | T_ICCP
	| T_PLTE
	| T_BKGD | T_HIST | T_TRNS | T_PHYS | T_SPLT | T_OFFS | T_PCAL | T_SCAL
	| T_IDAT
	| T_TIME | T_TEXT | T_ZTXT | T_ITXT | T_GIFG | T_GIFT | T_GIFX | T_FRAC
	| T_IEND
	| T_Others ByteString
	deriving (Eq, Show)

chunkType :: Chunk -> ChunkType
chunkType (ChunkIHDR _) = T_IHDR
chunkType (ChunkCHRM _) = T_CHRM
chunkType (ChunkGAMA _) = T_GAMA
chunkType (ChunkSRGB _) = T_SRGB
chunkType (ChunkPLTE _) = T_PLTE
chunkType (ChunkBKGD _) = T_BKGD
chunkType (ChunkIDAT _) = T_IDAT
chunkType (ChunkTEXT _) = T_TEXT
chunkType (ChunkIEND _) = T_IEND
chunkType (Others "sBIT" _) = T_SBIT
chunkType (Others "iCCP" _) = T_ICCP
chunkType (Others "hIST" _) = T_HIST
chunkType (Others "tRNS" _) = T_TRNS
chunkType (Others "pHYs" _) = T_PHYS
chunkType (Others "sPLT" _) = T_SPLT
chunkType (Others "oFFs" _) = T_OFFS
chunkType (Others "pCAL" _) = T_PCAL
chunkType (Others "sCAL" _) = T_SCAL
chunkType (Others "tIME" _) = T_TIME
chunkType (Others "tEXt" _) = T_ZTXT
chunkType (Others "iTXt" _) = T_ITXT
chunkType (Others "gIFg" _) = T_GIFG
chunkType (Others "gIFt" _) = T_GIFT
chunkType (Others "gIFx" _) = T_GIFX
chunkType (Others "fRAc" _) = T_FRAC
chunkType (Others n _) = T_Others n

ihdr :: [Chunk] -> IHDR
ihdr = (\(ChunkIHDR i) -> i) . head . filter ((== T_IHDR) . chunkType)

bplte :: [Chunk] -> [Chunk]
bplte = filter ((`elem` beforePLTEs) . chunkType)

plte :: [Chunk] -> Maybe PLTE
plte c = do
	ChunkPLTE pl <- find ((== T_PLTE) . chunkType) c
	return pl

bidat :: [Chunk] -> [Chunk]
bidat = filter ((`elem` beforeIDATs) . chunkType)

idats :: [Chunk] -> ByteString
idats = concatIDATs . body'

body' :: [Chunk] -> [IDAT]
body' cs = map cidat $ filter ((== T_IDAT) . chunkType) cs

concatIDATs :: [IDAT] -> ByteString
concatIDATs = concat . map idat_body

aplace :: [Chunk] -> [Chunk]
aplace = filter ((`elem` anyplaces) . chunkType)

others :: [Chunk] -> [Chunk]
others = filter $
	(`notElem` needs ++ beforePLTEs ++ beforeIDATs ++ anyplaces) . chunkType

needs :: [ChunkType]
needs = [T_IHDR, T_PLTE, T_IDAT, T_IEND]

beforePLTEs :: [ChunkType]
beforePLTEs = [T_CHRM, T_GAMA, T_SBIT, T_SRGB, T_ICCP]

beforeIDATs :: [ChunkType]
beforeIDATs = [T_BKGD, T_HIST, T_TRNS, T_PHYS, T_SPLT, T_OFFS, T_PCAL, T_SCAL]

anyplaces :: [ChunkType]
anyplaces = [T_TIME, T_TEXT, T_ZTXT, T_ITXT, T_GIFG, T_GIFT, T_GIFX, T_FRAC]
-- anyplaces = ["tIME", "tEXt", "zTXt", "iTXt", "gIFg", "gIFt", "gIFx", "fRAc"]

{-
nameTable :: [(ChunkType, ByteString)]
nameTable = [
	(T_IHDR, "IHDR"),
	(T_CHRM, "cHRM"),
	(T_
 ]
-}

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

[binary|

IHDR deriving Show

4: width
4: height
1: depth
: False
: False
: False
: False
: False
{Bool}: alpha
{Bool}: color
{Bool}: palet
1: compressionType
1: filterType
1: interlaceType

|]

[binary|

GAMA deriving Show

4: gamma

|]

[binary|

SRGB deriving Show

1: srgb

|]

[binary|

CHRM

deriving Show

arg :: Int

(4, Just (arg `div` 4)){[Int]}: chrms

|]

[binary|

PLTE deriving Show

arg :: Int

((), Just (arg `div` 3)){[(Int, Int, Int)]}: colors

|]

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	toBinary _ (b, g, r) = mconcat [toBinary 1 b, toBinary 1 g, toBinary 1 r]
	fromBinary _ s = do
		(r, rest) <- fromBinary 1 s
		(g, rest') <- fromBinary 1 rest
		(b, rest'') <- fromBinary 1 rest'
		return ((r, g, b), rest'')

[binary|

BKGD deriving Show

1: bkgd

|]

[binary|

IDAT deriving Show

arg :: Int

arg{ByteString}: idat_body

|]

[binary|

TEXT deriving Show

arg :: Int

((), Just arg){String}: text

|]

[binary|IEND deriving Show|]
