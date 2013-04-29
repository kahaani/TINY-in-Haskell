module Main where

import Scan(scan)
import Parse(parse)
import TypeCheck(typeCheck)
import BuildSymtab(buildSymtab,testSymtab)

main :: IO ()
main = do
	contents <- getContents
	--print . scan $ contents
	--mapM_ (putStrLn . show) (scan contents)
	--print . typeCheck . parse . scan $ contents
	print . testSymtab . typeCheck . parse . scan $ contents


