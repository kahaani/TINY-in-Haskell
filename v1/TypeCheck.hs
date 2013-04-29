module TypeCheck (
	typeCheck
) where

import Scan (Token(..))
import Parse (Stmt(..), Exp(..))

data ExpType = VoidT | IntegerT | BooleanT deriving (Eq)

typeCheck :: [Stmt] -> [Stmt]
typeCheck stmts = case typeCheckStmts stmts of
	True  -> stmts
	False -> error "There is a bug in type checking"

typeCheckStmts :: [Stmt] -> Bool
typeCheckStmts = and . map typeCheckStmt

typeCheckStmt :: Stmt -> Bool
typeCheckStmt (If2K exp stmts) =
	if   typeCheckExp exp /= BooleanT
	then error "Type Error: if test is not Boolean"
	else typeCheckStmts stmts
typeCheckStmt (If3K exp stmts1 stmts2) =
	if   typeCheckExp exp /= BooleanT
	then error "Type Error: if test is not Boolean"
	else typeCheckStmts stmts1 && typeCheckStmts stmts2
typeCheckStmt (RepeatK stmts exp) =
	if   typeCheckExp exp /= BooleanT
	then error "Type Error: repeat test is not Boolean"
	else typeCheckStmts stmts
typeCheckStmt (AssignK _ exp) =
	if typeCheckExp exp /= IntegerT
	then error "Type Error: assignment of non-integer value"
	else True
typeCheckStmt (WriteK exp) =
	if typeCheckExp exp /= IntegerT
	then error "Type Error: write of non-integer value"
	else True
typeCheckStmt (ReadK _) = True

typeCheckExp :: Exp -> ExpType
typeCheckExp (ConstK _) = IntegerT
typeCheckExp (IdK _)    = IntegerT
typeCheckExp (OpK token exp1 exp2) =
	if typeCheckExp exp1 /= IntegerT || typeCheckExp exp2 /= IntegerT
	then error "Type Error: Op applied to non-integer"
	else case token of
		Lt    -> BooleanT
		Equal -> BooleanT
		Plus  -> IntegerT
		Minus -> IntegerT
		Times -> IntegerT
		Over  -> IntegerT

