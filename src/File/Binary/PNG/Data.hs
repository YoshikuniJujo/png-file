{-# LANGUAGE TypeFamilies #-}

module File.Binary.PNG.Data (
	PNG(..), PNGImage(..)
) where

import Data.ByteString.Lazy

data PNG pi = PNG PNGHeader PNGValues [(String, ByteString)] pi

data PNGHeader = PNGHeader {
 }

data PNGValues = PNGValues {
 }

class PNGImage pi where
	type PNGColor pi
