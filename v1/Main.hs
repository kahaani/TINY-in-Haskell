module Main where

import Scan(scan)
import Parse(parse)

main :: IO ()
main = do
	contents <- getContents
	--print . scan $ contents
	print . parse . scan $ contents


