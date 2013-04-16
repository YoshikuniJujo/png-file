{-# LANGUAGE OverloadedStrings #-}

module File.Binary.PNG.Chunks.CRC (crc, checkCRC) where

import Data.Array
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8()

--------------------------------------------------------------------------------

crc :: BSL.ByteString -> BSL.ByteString
crc = BSL.reverse . word32ToWord8s . xor 0xffffffff . BSL.foldl crc' 0xffffffff
	where
	crc' :: Word32 -> Word8 -> Word32
	crc' c x = table ! i `xor` shiftR c 8
		where
		i = (c `xor` fromIntegral x) .&. 0xff

table :: Array Word32 Word32
table = listArray (0, 255) $ map (\n -> foldl table' n [0 .. 7]) [0 .. 255]

table' :: Word32 -> Int -> Word32
table' c _
	| c .&. 1 == 0 = shiftR c 1
	| otherwise = xor 0xedb88320 $ shiftR c 1

checkCRC :: BSL.ByteString -> BSL.ByteString -> Bool
checkCRC str c = crc (str `BSL.append` BSL.reverse c) == "\x21\x44\xdf\x1c"

word32ToWord8s :: Word32 -> BSL.ByteString
word32ToWord8s 0 = ""
word32ToWord8s w = fromIntegral (w .&. 0xff) `BSL.cons'` word32ToWord8s (w `shiftR` 8)
