{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BSL
import DrawBitmap

lineData = "\xff\x00\x00" `BSL.append` "\xf0\x00\x00"

line0 = BSL.take 150 $ BSL.cycle lineData
testLine = BSL.take 150 $ BSL.cycle lineData
-- testData = BSL.take 7550 $ BSL.cycle $ BSL.cons 1 testLine
testData = BSL.take 7550 $ BSL.cycle $ BSL.cons 2 testLine

main = drawBitmap 50 50 testData
