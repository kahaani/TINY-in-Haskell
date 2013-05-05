module TypeCheck (
	typeCheck
) where

import Control.Monad.Error
import Scan (Token(..))
import Parse (Stmt(..), Exp(..))

data ExpType = VoidT | IntegerT | BooleanT deriving (Eq)

typeCheck :: [Stmt] -> [Stmt]
typeCheck stmts = case typeCheckStmts stmts of
	Right () -> stmts
	Left err -> error $ "Type Error: " ++ err

typeCheckStmts :: [Stmt] -> Either String ()
typeCheckStmts = mapM_ typeCheckStmt

typeCheckStmt :: Stmt -> Either String ()
typeCheckStmt (If2K exp stmts) = do
	type1 <- typeCheckExp exp
	assert (type1 == BooleanT) "if test is not Boolean"
	typeCheckStmts stmts
typeCheckStmt (If3K exp stmts1 stmts2) = do
	type1 <- typeCheckExp exp
	assert (type1 == BooleanT) "if test is not Boolean"
	typeCheckStmts stmts1
	typeCheckStmts stmts2
typeCheckStmt (RepeatK stmts exp) = do
	type1 <- typeCheckExp exp
	assert (type1 == BooleanT) "repeat test is not Boolean"
	typeCheckStmts stmts
typeCheckStmt (AssignK _ exp) = do
	type1 <- typeCheckExp exp
	assert (type1 == IntegerT) "assignment of non-integer value"
typeCheckStmt (WriteK exp) = do
	type1 <- typeCheckExp exp
	assert (type1 == IntegerT) "write of non-integer value"
typeCheckStmt (ReadK _) = do
	return ()

typeCheckExp :: Exp -> Either String ExpType
typeCheckExp (ConstK _) = return IntegerT
typeCheckExp (IdK _)    = return IntegerT
typeCheckExp (OpK token exp1 exp2) = do
	type1 <- typeCheckExp exp1
	assert (type1 == IntegerT) "Op applied to non-integer"
	type2 <- typeCheckExp exp2
	assert (type2 == IntegerT) "Op applied to non-integer"
	case token of
		Lt    -> return BooleanT
		Equal -> return BooleanT
		Plus  -> return IntegerT
		Minus -> return IntegerT
		Times -> return IntegerT
		Over  -> return IntegerT

assert :: Bool -> String -> Either String ()
assert True  _   = return ()
assert False msg = Left msg

