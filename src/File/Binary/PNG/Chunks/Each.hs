{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}

module File.Binary.PNG.Chunks.Each (
	IHDR(..), PLTE(..), RGB8(..), IDAT(..), IEND(..),
	TRNS,
	CHRM(..), GAMA(..), ICCP(..), SBIT, SRGB(..),
	ITXT, TEXT(..), ZTXT,
	BKGD(..), HIST, PHYS, SPLT, TIME,
	DATA(..),

	chunkNames, critical, beforePLTE, beforeIDAT, anyPlace
) where

import Data.Monoid (mconcat)
import Data.ByteString.Lazy (ByteString, append)
import File.Binary (binary, Field(..), Binary(..))
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.Instances.MSB0 ()
import qualified Data.ByteString.Lazy.Char8 as BSLC

--------------------------------------------------------------------------------

chunkNames, critical, beforePLTE, beforeIDAT, anyPlace :: [String]
chunkNames = critical ++ beforePLTE ++ beforeIDAT ++ anyPlace
critical = ["IHDR", "PLTE", "IDAT", "IEND"]
beforePLTE = ["cHRM", "gAMA", "iCCP", "sBIT", "sRGB", "bKGD", "hIST", "tRNS"]
beforeIDAT = ["pHYs", "sPLT"]
anyPlace = ["tIME", "iTXt", "tEXt", "zTXt"]

type TRNS = DATA
-- type ICCP = DATA
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
	toBinary () RGB8{ red = r, green = g, blue = b } = do
		r' <- toBinary 1 r
		g' <- toBinary 1 g
		b' <- toBinary 1 b
		return $ mconcat [r', g', b']
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

ICCP deriving Show

arg :: Int

{NullString}: iccp_name
1: iccp_con
(arg - length (nullString iccp_name) - 2){ByteString}: iccp_body

|]

data NullString = NullString { nullString :: String } deriving Show

instance Field NullString where
	type FieldArgument NullString = ()
	toBinary () (NullString str) =
		return $ makeBinary $ (`append` "\NUL") $ BSLC.pack str
	fromBinary () bin = do
		let (ret, rest) = spanBytes (/= 0) bin
		return (NullString $ BSLC.unpack ret, snd $ unconsByte rest)

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

arg{ByteString}: bkgd

|]

[binary|

DATA deriving Show

arg :: Int

arg{ByteString}: dat

|]
