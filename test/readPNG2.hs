import File.Binary.PNG
import System.Environment

main :: IO ()
main = do
	fin : _ <- getArgs
	cnt <- readBinaryFile fin
	let p = readPNG' cnt
	putStrLn $ take 1000 $ show p
