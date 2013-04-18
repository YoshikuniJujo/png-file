{-# LANGUAGE OverloadedStrings #-}

module File.Binary.PNG.Chunks.CRC (checkCRC, crc) where

import Prelude hiding (reverse)
import Data.Array (Array, listArray, (!))
import Data.Word (Word32)
import Data.Bits ((.&.), xor, shiftR)
import Data.ByteString.Lazy (ByteString, cons', append, reverse)
import qualified Data.ByteString.Lazy as BSL (foldl)
import Data.ByteString.Lazy.Char8 ()

--------------------------------------------------------------------------------

checkCRC :: ByteString -> ByteString -> Bool
checkCRC str c = crc (str `append` reverse c) == "\x21\x44\xdf\x1c"

crc :: ByteString -> ByteString
crc = reverse . word32ToBS . xor 0xffffffff . BSL.foldl crc' 0xffffffff
	where
	crc' c x = table ! (c .&. 0xff `xor` fromIntegral x) `xor` shiftR c 8

table :: Array Word32 Word32
table = listArray (0, 255) $ map (\n -> iterate crc8bit n !! 8) [0 .. 255]

crc8bit :: Word32 -> Word32
crc8bit c
	| c .&. 1 == 0 = shiftR c 1
	| otherwise = 0xedb88320 `xor` shiftR c 1

word32ToBS :: Word32 -> ByteString
word32ToBS 0 = ""
word32ToBS w = fromIntegral (w .&. 0xff) `cons'` word32ToBS (w `shiftR` 8)
