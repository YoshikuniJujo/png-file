{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DrawBitmap (drawBitmap, bsToImage, drawBitmap2) where

import Prelude hiding (null, take, drop, replicate, cycle, length)
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Data.Bits
import Data.Char
import Control.Monad
import Data.ByteString.Lazy (
	ByteString, null, unpack, take, drop, replicate, cycle, length, uncons,
	append, pack)
import Data.ByteString.Lazy.Char8 ()
import Data.Word
import Data.Int
import Data.Maybe
import Control.Concurrent
import Data.Time

openWindow :: IO (Display, Window, GC)
openWindow = do
	_ <- initThreads
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let scr = defaultScreen dpy
	root <- rootWindow dpy scr
	win <- createSimpleWindow dpy root 0 0 100 100 1
		(blackPixel dpy scr) (whitePixel dpy scr)
	gc <- createGC dpy win
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask .|. pointerMotionMask
	mapWindow dpy win
	return (dpy, win, gc)

drawBitmap :: Position -> Position -> ByteString -> IO ()
drawBitmap w h dat = do
	(dpy, win, gc) <- openWindow
--	flush dpy
	thread <- newChan
	(writeChan thread =<<) $ forkIO $ do
		t0 <- getCurrentTime
		print t0
		image dpy win gc w 0 0 0 0
			(replicate (fromIntegral w * 3 + 3) 0) dat
		print . (`diffUTCTime` t0) =<< getCurrentTime
	loop' dpy win gc w h dat thread

-- drawBitmap2 :: Position -> Position -> ByteString -> IO ()
drawBitmap2 image = do
	(dpy, win, gc) <- openWindow
--	flush dpy
	thread <- newChan
	(writeChan thread =<<) $ forkIO $ do
		t0 <- getCurrentTime
		print t0
--		image dpy win gc w 0 0 0 0
--			(replicate (fromIntegral w * 3 + 3) 0) dat
		print . (`diffUTCTime` t0) =<< getCurrentTime
	loop'' dpy win gc image thread
	where
--	image = bsToImage w dat -- (replicate (fromIntegral w * 3 + 3) 0) dat

drawBitmap' :: Display -> Window -> GC -> Position -> [[(Word8, Word8, Word8)]] -> IO ()
drawBitmap' _ _ _ _ [] = return ()
drawBitmap' dpy win gc y (l : ls) =
	drawBitmapLine dpy win gc y 0 l >> drawBitmap' dpy win gc (y + 1) ls

-- drawBitmapLine :: Position -> Position -> [(Word8, Word8, Word8)] -> IO ()
drawBitmapLine _ _ _ _ _ [] = return ()
drawBitmapLine dpy win gc y x (c : cs) = do
	setForeground dpy gc $ rgbToWord32 $ (\(r, g, b) -> [r, g, b]) c
	drawPoint dpy win gc x y
	drawBitmapLine dpy win gc y (x + 1) cs

loop' :: Display -> Window -> GC -> Position -> Position -> ByteString -> Chan ThreadId -> IO ()
loop' dpy win gc w h dat thread = allocaXEvent $ \e -> do
    threadDelay 100000
    pend <- pending dpy
    if (pend > 0) then do
	nextEvent dpy e
	ev <- getEvent e
	case ev of
		KeyEvent {} -> do
			ch <- fmap (chr . fromEnum) $ keycodeToKeysym dpy
				(ev_keycode ev) 0
			if ch == 'q' then return () else loop' dpy win gc w h dat thread
		ExposeEvent {} -> do
			readChan thread >>= \th -> do
				killThread th
				print th
			(writeChan thread =<<) $ forkIO $ do
				t0 <- getCurrentTime
				print t0
				image dpy win gc w 0 0 0 0
					(replicate (fromIntegral w * 3 + 3) 0) dat
				print . (`diffUTCTime` t0) =<< getCurrentTime
			loop' dpy win gc w h dat thread
		MotionEvent {} -> do
			return ()
			loop' dpy win gc w h dat thread
		_ -> print ev
	else loop' dpy win gc w h dat thread

-- loop'' :: Display -> Window -> GC -> Position -> Position -> ByteString -> Chan ThreadId -> IO ()
loop'' dpy win gc image thread = allocaXEvent $ \e -> do
    threadDelay 100000
    pend <- pending dpy
    if (pend > 0) then do
--	print image
	nextEvent dpy e
	ev <- getEvent e
	case ev of
		KeyEvent {} -> do
			ch <- fmap (chr . fromEnum) $ keycodeToKeysym dpy
				(ev_keycode ev) 0
			if ch == 'q' then return () else loop'' dpy win gc image thread
		ExposeEvent {} -> do
			readChan thread >>= \th -> do
				killThread th
				print th
			(writeChan thread =<<) $ forkIO $ do
				t0 <- getCurrentTime
				print t0
				drawBitmap' dpy win gc 0 $ image
--					bsToImage w (replicate
--						(fromIntegral w * 3 + 3) 0) dat []
{-
				image dpy win gc w 0 0 0 0
					(replicate (fromIntegral w * 3 + 3) 0) dat
-}
				print . (`diffUTCTime` t0) =<< getCurrentTime
			loop'' dpy win gc image thread
		MotionEvent {} -> do
			return ()
			loop'' dpy win gc image thread
		_ -> print ev
	else loop'' dpy win gc image thread

main :: IO ()
main = do
	(dpy, win, gc) <- openWindow
	flush dpy
	loop dpy win gc
--	getLine >> return ()

loop :: Display -> Window -> GC -> IO ()
loop dpy win gc = allocaXEvent $ \e -> do
	nextEvent dpy e
	ev <- getEvent e
--	print ev
	case ev of
		KeyEvent {} -> do
			ch <- fmap (chr . fromEnum) $ keycodeToKeysym dpy
				(ev_keycode ev) 0
			if ch == 'q' then return () else loop dpy win gc
		_ -> do	image dpy win gc 50 100 100 0 0 "" $ take (2500 * 3) $
				cycle "\xff\x00\xff" -- replicate (2500 * 3) 0
{-
		_ -> do	forM_ [0 .. 50 * 50 - 1] $ \i -> do
				setForeground dpy gc 0xff0000
				drawPoint dpy win gc (100 + i `mod` 50) (100 + i `div` 50)
-}
			loop dpy win gc

drawL dpy win gc filter w x0 y0 x y pre left dat
--	| null dat = return ""
	| x < w = do
		let	l = fromMaybe [0, 0, 0] left
			lu = maybe [0, 0, 0] (const $ take' 3 pre) left
			color = getColor filter
				l (take' 3 $ drop 3 pre) lu (take' 3 dat)
			color' = take' 3 pre
--		print filter
--		print l
--		print color
		setForeground dpy gc $ rgbToWord32 color
		drawPoint dpy win gc (x0 + x) (y0 + y)
		drawL dpy win gc filter w x0 y0 (x + 1) y
			(setpre w pre $ pack color) (Just $ color) $ drop 3 dat
	| otherwise = return (pre, dat)

bsToLine :: Word8 -> Position -> Position -> ByteString ->
	Maybe (Word8, Word8, Word8) ->
	[(Word8, Word8, Word8)] -> ByteString ->
	(ByteString, [(Word8, Word8, Word8)], ByteString)
bsToLine filter w x pre left ret dat
	| x < w = let
		tToL (r, g, b) = [r, g, b]
		lToT [r, g, b] = (r, g, b)
		l = fromMaybe (0, 0, 0) left
		lu = (\[r, g, b] -> (r, g, b)) $
			maybe [0, 0, 0] (const $ take' 3 pre) left
		color'@[r, g, b] = getColor filter
			(tToL l) (take' 3 $ drop 3 pre) (tToL lu) (take' 3 dat)
		color = (r, g, b) in
		bsToLine filter w (x + 1) (setpre w pre $ pack color')
			(Just color) (color : ret) $ drop 3 dat
	| otherwise = (pre, ret, dat)

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 _ [] [] [] [] = []
zipWith4 f (x : xs) (y : ys) (z : zs) (w : ws) = f x y z w : zipWith4 f xs ys zs ws

getColor 0 left up leftup rgb = rgb
getColor 1 left up leftup rgb = zipWith (+) left rgb
getColor 2 left up leftup rgb = zipWith (+) up rgb
getColor 4 left up leftup rgb = zipWith4 getByte4' left up leftup rgb

sub :: Word8 -> Word8 -> Word8
sub x y	| x > y = x - y
	| otherwise = y - x

paeth' :: Word8 -> Word8 -> Word8 -> Word8
paeth' a b c = let
	[a', b', c'] = map fromIntegral [a, b, c]
	p :: Int = a' + b' - c'
	pa = abs $ p - a'
	pb = abs $ p - b'
	pc = abs $ p - c' in
	if pa <= pb && pa <= pc then a else
		if pb <= pc then b else c

paeth :: Int -> Int -> Int -> Int
paeth a b c = let
	p = a + b - c
	pa = abs $ p - a
	pb = abs $ p - b
	pc = abs $ p - c in
	if pa <= pb && pa <= pc then a else
		if pb <= pc then b else c

getByte4'' :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
getByte4'' left up leftup rgb = paeth' left up leftup + rgb

getByte4' :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
getByte4' left up leftup rgb = fromIntegral (paeth (fi left) (fi up) (fi leftup)) + rgb
	where
	fi = fromIntegral

getByte4 :: Int -> Int -> Int -> Int -> Int
getByte4 left up leftup rgb = let
	x = left + up - leftup
	a = abs $ x - left
	b = abs $ x - up
	c = abs $ x - leftup in
	if a <= b && a <= c then
		left + rgb else if b <= c then
			up + rgb else
				leftup + rgb

setpre :: Position -> ByteString -> ByteString -> ByteString
setpre w pre rgb
	| fromIntegral (length pre) == w * 3 + 3 = drop 3 pre `append` rgb
	| otherwise = error "bad pre" -- pre `append` rgb

bsToImage w bs = reverse $ map reverse $
	bsToImage' w (replicate (fromIntegral w * 3 + 3) 0) bs []
--	image = bsToImage w (replicate (fromIntegral w * 3 + 3) 0) dat

-- bsToLine filter w x pre left ret dat
bsToImage' :: Position -> ByteString -> ByteString -> [[(Word8, Word8, Word8)]] ->
	[[(Word8, Word8, Word8)]]
bsToImage' w pre bs rets
	| null bs = rets
	| otherwise = let
		Just (filter, dat') = uncons bs
		(pre', ret, dat'') = bsToLine filter w 0 pre Nothing [] dat' in
		bsToImage' w pre' dat'' (ret : rets)

-- image :: Display -> Window -> GC -> Position -> Position -> Position -> ByteString -> IO ()
image dpy win gc w x0 y0 x y pre dat
	| null dat = return ()
	| otherwise = do
--		print pre
--		print $ length dat
		let Just (filter, dat') = uncons dat
--		print filter
--		when (filter == 0) $ error "filter = 0"
		when (filter == 1) $ print filter
		(pre', dat'') <- drawL dpy win gc filter w x0 y0 x y pre Nothing dat'
{-
		setForeground dpy gc $ rgbToWord32 $ take' 3 dat
		drawPoint dpy win gc (x0 + x) (y0 + y)
		let (x', y') = if x == w - 1 then (0, y + 1) else (x + 1, y)
-}
		image dpy win gc w x0 y0 x (y + 1) pre' dat'' -- $ drop 3 dat

take' :: Int64 -> ByteString -> [Word8]
take' n = unpack . take n

rgbToWord32 :: [Word8] -> Word32
rgbToWord32 rgb = r `shiftL` 16 .|. g `shiftL` 8 .|. b
	where
	[r, g, b] = map fromIntegral rgb
