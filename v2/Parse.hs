module Parse (
	Stmt(..),
	Exp(..),
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

newtype ParseM a = ParseM {
		runParse :: [Token] -> (a, [Token])
	}

instance Monad ParseM where
	return x = ParseM $ \s -> (x,s)
	(ParseM h) >>= f = ParseM $ \s-> let
			(res, newstate) = h s
			(ParseM g) = f res
		in g newstate


parse :: [Token] -> [Stmt]
parse text = let
		(stmts, rest) = (runParse stmt_sequence) text
	in case rest of
		[Endfile] -> stmts
		otherwise -> error "Parse Error: expected EOF"

stmt_sequence :: ParseM [Stmt]
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

statement :: ParseM Stmt
statement = do
	ahead <- lookaheadM
	case ahead of
		If     -> if_stmt
		Repeat -> repeat_stmt
		Id _   -> assign_stmt
		Read   -> read_stmt
		Write  -> write_stmt

if_stmt :: ParseM Stmt
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
		otherwise -> fail "Parse Error: expect \"else\" or \"end\""

repeat_stmt :: ParseM Stmt
repeat_stmt = do
	matchM Repeat
	res1 <- stmt_sequence
	matchM Until
	res2 <- expression
	return $ RepeatK res1 res2

assign_stmt :: ParseM Stmt
assign_stmt = do
	res1 <- matchIdM
	matchM Assign
	res2 <- expression
	return $ AssignK res1 res2

read_stmt :: ParseM Stmt
read_stmt = do
	matchM Read
	res <- matchIdM
	return $ ReadK res

write_stmt :: ParseM Stmt
write_stmt = do
	matchM Write
	res <- expression
	return $ WriteK res

expression :: ParseM Exp
expression = do
	res <- simple_exp
	expression_recursive res

expression_recursive :: Exp -> ParseM Exp
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

simple_exp :: ParseM Exp
simple_exp = do
	res <- term
	simple_exp_recursive res

simple_exp_recursive :: Exp -> ParseM Exp
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

term :: ParseM Exp
term = do
	res <- factor
	term_recursive res

term_recursive :: Exp -> ParseM Exp
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

factor :: ParseM Exp
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
		otherwise -> fail "Parse Error: expected \"(\" or number or identifier"

lookaheadM :: ParseM Token
lookaheadM = ParseM $ lookahead

lookahead :: [Token] -> (Token, [Token])
lookahead text@(x : _) = (x, text)
--lookahead _ = undefined

matchM :: Token -> ParseM ()
matchM t = ParseM $ match t

match :: Token -> [Token] -> ((), [Token])
match y (x:rest)
	| y==x = ((), rest)
	| otherwise = error $ "Parse Error: expected " ++ show y ++ ", but got " ++ show x
match y _ = error $ "Parse Error: expected " ++ show y

matchIdM :: ParseM String
matchIdM = ParseM matchId

matchId :: [Token] -> (String, [Token])
matchId (Id name:rest) = (name, rest)
matchId (x:rest) = error $ "Parse Error: expected identifier, but got " ++ show x
matchId _ = error "Parse Error: expected identifier"

matchNumM :: ParseM Int
matchNumM = ParseM matchNum

matchNum :: [Token] -> (Int, [Token])
matchNum (Num value:rest) = (value, rest)
matchNum (x:rest) = error $ "Parse Error: expected number, but got " ++ show x
matchNum _ = error "Parse Error: expected number"

