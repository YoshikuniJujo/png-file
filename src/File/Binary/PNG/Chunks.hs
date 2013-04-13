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
) where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import Data.ByteString.Lazy
import Data.Monoid

data Chunk
	= ChunkIHDR IHDR
	| ChunkCHRM CHRM
	| ChunkGAMA GAMA
	| ChunkSRGB SRGB
	| ChunkPLTE PLTE
	| ChunkBKGD BKGD
	| ChunkIDAT { cidat :: IDAT }
	| ChunkTEXT TEXT
	| ChunkIEND IEND
	| Others ByteString ByteString
	deriving Show

data ChunkType
	= T_IHDR
	| T_CHRM
	| T_GAMA
	| T_SBIT
	| T_SRGB
	| T_ICCP
	| T_PLTE
	| T_BKGD
	| T_IDAT
	| T_TEXT
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
chunkType (Others n _) = T_Others n

beforePLTEs :: [ChunkType]
beforePLTEs = [T_CHRM, T_GAMA, T_SBIT, T_SRGB, T_ICCP]

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
name (Others n _) = n
name _ = error "yet"

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

arg{ByteString}: idat

|]

[binary|

TEXT deriving Show

arg :: Int

((), Just arg){String}: text

|]

[binary|IEND deriving Show|]
