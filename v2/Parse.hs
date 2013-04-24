module Parse (
	parse
) where

import Control.Monad
import Scan (Token(..))

data Stmt = 
	  If2K Exp [Stmt]
	| If3K Exp [Stmt] [Stmt]
	| RepeatK [Stmt] Exp
	| AssignK String Exp --Assign Token/IDK
	| ReadK String  --ReadK Token/IDK
	| WriteK Exp
	deriving (Show)

data Exp =
	  OpK Token Exp Exp
	| ConstK Int  --ConstK Token
	| IdK String  --IdK Token
	deriving (Show)

-- data ExpType = VoidT | IntegerT | BooleanT

data Parse a = Parse {
		runParse :: [Token] -> (a, [Token])
	}

instance Monad Parse where
	return x = Parse $ \s -> (x,s)
	(Parse h) >>= f = Parse $ \s-> let
			(res, newstate) = h s
			(Parse g) = f res
		in g newstate


parse :: [Token] -> [Stmt]
parse text = let
		(stmts, rest) = (runParse stmt_sequence) text
	in case rest of
		[Endfile] -> stmts
		otherwise -> error "Parse Error: expected EOF"

stmt_sequence :: Parse [Stmt]
stmt_sequence = do
	stmt <- statement
	ahead <- lookaheadM
	case ahead of
		Semi -> do
			matchM Semi
			stmts <- stmt_sequence
			return $ stmt:stmts
		otherwise -> do
			return $ [stmt]

statement :: Parse Stmt
statement = do
	ahead <- lookaheadM
	case ahead of
		If -> if_stmt
		Repeat -> repeat_stmt
		Id _ -> assign_stmt
		Read -> read_stmt
		Write -> write_stmt

if_stmt :: Parse Stmt
if_stmt = do
	matchM If
	res1 <- expression
	matchM Then
	res2 <- stmt_sequence
	ahead <- lookaheadM
	case ahead of
		Else -> do
			matchM Else
			res3 <- stmt_sequence
			matchM End
			return $ If3K res1 res2 res3
		End -> do
			matchM End
			return $ If2K res1 res2
		otherwise -> error "Parse Error: expect \"else\" or \"end\""

repeat_stmt :: Parse Stmt
repeat_stmt = do
	matchM Repeat
	res1 <- stmt_sequence
	matchM Until
	res2 <- expression
	return $ RepeatK res1 res2

assign_stmt :: Parse Stmt
assign_stmt = do
	res1 <- matchIdM
	matchM Assign
	res2 <- expression
	return $ AssignK res1 res2

read_stmt :: Parse Stmt
read_stmt = do
	matchM Read
	res <- matchIdM
	return $ ReadK res

write_stmt :: Parse Stmt
write_stmt = do
	matchM Write
	res <- expression
	return $ WriteK res

expression :: Parse Exp
expression = do
	res <- simple_exp
	expression_recursive res

expression_recursive :: Exp -> Parse Exp
expression_recursive lh = do
	ahead <- lookaheadM
	case ahead of
		Lt -> do
			matchM Lt
			res <- simple_exp
			expression_recursive (OpK Lt lh res)
		Equal -> do
			matchM Equal
			res <- simple_exp
			expression_recursive (OpK Equal lh res)
		otherwise -> do
			return lh

simple_exp :: Parse Exp
simple_exp = do
	res <- term
	simple_exp_recursive res

simple_exp_recursive :: Exp -> Parse Exp
simple_exp_recursive lh = do
	ahead <- lookaheadM
	case ahead of
		Plus -> do
			matchM Plus
			res <- term
			simple_exp_recursive (OpK Plus lh res)
		Minus -> do
			matchM Minus
			res <- term
			simple_exp_recursive (OpK Minus lh res)
		otherwise -> do
			return lh

term :: Parse Exp
term = do
	res <- factor
	term_recursive res

term_recursive :: Exp -> Parse Exp
term_recursive lh = do
	ahead <- lookaheadM
	case ahead of
		Times -> do
			matchM Times
			res <- factor
			term_recursive (OpK Times lh res)
		Over -> do
			matchM Over
			res <- factor
			term_recursive (OpK Over lh res)
		otherwise -> do
			return lh

factor :: Parse Exp
factor = do
	ahead <- lookaheadM
	case ahead of
		Lparen -> do
			matchM Lparen
			res <- expression
			matchM Rparen
			return res
		Id _ -> do
			res <- matchIdM
			return $ IdK res
		Num _ -> do
			res <- matchNumM
			return $ ConstK res
		otherwise -> error "Parse Error: expected \"(\" or number or identifier"

lookahead :: [Token] -> (Token, [Token])
lookaheadM :: Parse Token
lookaheadM = Parse $ lookahead

lookahead text@(x : _) = (x, text)
--lookahead _ = undefined

matchM :: Token -> Parse ()
matchM t = Parse $ match t

match :: Token -> [Token] -> ((), [Token])
match y (x:rest)
	| y==x = ((), rest)
	| otherwise = error $ "Parse Error: expected " ++ show y ++ ", but got " ++ show x
match y _ = error $ "Parse Error: expected " ++ show y

matchIdM :: Parse String
matchIdM = Parse matchId

matchId :: [Token] -> (String, [Token])
matchId (Id name:rest) = (name, rest)
matchId (x:rest) = error $ "Parse Error: expected identifier, but got " ++ show x
matchId _ = error "Parse Error: expected identifier"

matchNumM :: Parse Int
matchNumM = Parse matchNum

matchNum :: [Token] -> (Int, [Token])
matchNum (Num value:rest) = (value, rest)
matchNum (x:rest) = error $ "Parse Error: expected number, but got " ++ show x
matchNum _ = error "Parse Error: expected number"

