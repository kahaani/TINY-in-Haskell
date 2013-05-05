module BuildSymtab (
	Variable,
	Location,
	Symtab,
	testSymtab, -- for debug
	buildSymtab
) where

import qualified Data.Map as Map

import Scan (Token(..))
import Parse (Stmt(..), Exp(..))

type Variable = String
type Location = Int
type Symtab   = Map.Map Variable Location

{-
	write monad: a -> a, log       (hiding log)
	state monad: state -> a, state (hiding state)
	buildSymtab: a, state -> state (hiding state)
-}

(>->) = flip (.)
infixr 9 >->

testSymtab :: [Stmt] -> [(Variable,Location)]
testSymtab stmts = Map.toList (bstStmts stmts Map.empty)

buildSymtab :: [Stmt] -> ([Stmt], Symtab)
buildSymtab stmts = (stmts, bstStmts stmts Map.empty)

bstStmts :: [Stmt] -> Symtab -> Symtab
bstStmts stmts st = foldl (\acc x -> bstStmt x acc) st stmts

bstStmt :: Stmt -> Symtab -> Symtab
bstStmt (If2K exp stmts)         = bstExp exp >-> bstStmts stmts
bstStmt (If3K exp stmts1 stmts2) = bstExp exp >-> bstStmts stmts1 >-> bstStmts stmts2
bstStmt (RepeatK stmts exp)      = bstStmts stmts >-> bstExp exp
bstStmt (AssignK iden exp)       = insert iden >-> bstExp exp
bstStmt (ReadK iden)             = insert iden
bstStmt (WriteK exp)             = bstExp exp

bstExp :: Exp -> Symtab -> Symtab
bstExp (OpK token exp1 exp2) = bstExp exp1 >-> bstExp exp2
bstExp (ConstK _) = id
bstExp (IdK iden) = insert iden

insert :: Variable -> Symtab -> Symtab
insert v st = case Map.lookup v st of
	Just _  -> st
	Nothing -> Map.insert v (Map.size st) st

