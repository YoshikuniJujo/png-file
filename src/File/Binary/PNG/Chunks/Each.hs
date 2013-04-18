{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module File.Binary.PNG.Chunks.Each (
	IHDR(..), PLTE(..), RGB8(..), IDAT(..), IEND(..),
	TRNS,
	CHRM(..), GAMA(..), ICCP, SBIT, SRGB(..),
	ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME,
	DATA,

	chunkNames, critical, beforePLTE, beforeIDAT, anyPlace
) where

import Data.Monoid (mconcat)
import Data.ByteString.Lazy (ByteString)
import File.Binary (binary, Field(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()

--------------------------------------------------------------------------------

chunkNames, critical, beforePLTE, beforeIDAT, anyPlace :: [String]
chunkNames = critical ++ beforePLTE ++ beforeIDAT ++ anyPlace
critical = ["IHDR", "PLTE", "IDAT", "IEND"]
beforePLTE = ["cHRM", "gAMA", "iCCP", "sBIT", "sRGB", "bKGD", "hIST", "tRNS"]
beforeIDAT = ["pHYs", "sPLT"]
anyPlace = ["tIME", "iTXt", "tEXt", "zTXt"]

type TRNS = DATA
type ICCP = DATA
type SBIT = DATA
type ITXT = DATA
type ZTXT = DATA
type HIST = DATA
type PHYS = DATA
type SPLT = DATA
type TIME = DATA

[binary|

IHDR deriving Show

arg :: Int

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

PLTE deriving Show

arg :: Int

((), Just (arg `div` 3)){[RGB8]}: colors

|]

data RGB8 = RGB8 { red :: Int, green :: Int, blue :: Int } deriving Show

instance Field RGB8 where
	type FieldArgument RGB8 = ()
	toBinary () RGB8{ red = r, green = g, blue = b } =
		mconcat [toBinary 1 r, toBinary 1 g, toBinary 1 b]
	fromBinary () bin = do
		(r, bin') <- fromBinary 1 bin
		(g, bin'') <- fromBinary 1 bin'
		(b, bin''') <- fromBinary 1 bin''
		return (RGB8{ red = r, green = g, blue = b } , bin''')

[binary|

IDAT deriving Show

arg :: Int

arg{ByteString}: idat_body

|]

[binary|IEND deriving Show arg :: Int|]

[binary|

CHRM deriving Show

arg :: Int

(4, Just (arg `div` 4)){[Int]}: chrms

|]

[binary|

GAMA deriving Show

arg :: Int

4: gamma

|]

[binary|

SRGB deriving Show

arg :: Int

1: srgb

|]

[binary|

TEXT deriving Show

arg :: Int

((), Just arg){String}: text

|]

[binary|

BKGD deriving Show

arg :: Int

arg: bkgd

|]

[binary|

DATA deriving Show

arg :: Int

arg{ByteString}: dat

|]
