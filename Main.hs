module Main where

import Scan(scan)

main :: IO ()
main = do
	contents <- getContents
	print . scan $ contents


