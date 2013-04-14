{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.PNG.Chunks.Structures where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import Data.Monoid
import Data.ByteString.Lazy

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

CHRM deriving Show

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

arg :: Int

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

[binary|IEND deriving Show arg :: Int|]

