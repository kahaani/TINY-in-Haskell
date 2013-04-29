module Main where

import Scan(scan)
import Parse(parse)
import TypeCheck(typeCheck)
import BuildSymtab(buildSymtab,testSymtab)
import CodeGen(codeGen)

main :: IO ()
main = do
	contents <- getContents
	mapM_ putStrLn . codeGen . buildSymtab . typeCheck . parse . scan $ contents
	--print . testSymtab . typeCheck . parse . scan $ contents
	--print . typeCheck . parse . scan $ contents
	--print . parse . scan $ contents
	--mapM_ print . scan $ contents
	--print . scan $ contents

