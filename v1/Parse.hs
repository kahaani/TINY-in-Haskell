module Parse (
	parse
) where

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


parse :: [Token] -> [Stmt]
parse text = let
		(stmts, rest) = stmt_sequence text
	in case rest of
		[Endfile] -> stmts
		otherwise -> error "Parse Error: expected EOF"

stmt_sequence :: [Token] -> ([Stmt], [Token])
stmt_sequence text = let
		(stmt,  rest1) = statement text
		rest2          = match Semi rest1
		(stmts, rest3) = stmt_sequence rest2
	in case rest1 of
		(Semi:_)  -> (stmt:stmts, rest3)
		otherwise -> ([stmt], rest1)

statement :: [Token] -> (Stmt, [Token])
statement text@(If    :rest) = if_stmt text
statement text@(Repeat:rest) = repeat_stmt text
statement text@(Id _  :rest) = assign_stmt text
statement text@(Read  :rest) = read_stmt text
statement text@(Write :rest) = write_stmt text

if_stmt :: [Token] -> (Stmt, [Token])
if_stmt text = let
		rest1            = match If text
		(result1, rest2) = expression rest1
		rest3            = match Then rest2
		(result2, rest4) = stmt_sequence rest3
		rest5'           = match End rest4
		rest5            = match Else rest4
		(result3, rest6) = stmt_sequence rest5
		rest7            = match End rest6
	in case rest4 of
		(Else:_)  -> (If3K result1 result2 result3, rest7 )
		(End :_)  -> (If2K result1 result2,         rest5')
		otherwise -> error "Parse Error: expect \"else\" or \"end\""

repeat_stmt :: [Token] -> (Stmt, [Token])
repeat_stmt text = let
		rest1            = match Repeat text
		(result1, rest2) = stmt_sequence rest1
		rest3            = match Until rest2
		(result2, rest4) = expression rest3
	in (RepeatK result1 result2, rest4)

assign_stmt :: [Token] -> (Stmt, [Token])
assign_stmt text = let
		(result1, rest1) = matchId text
		rest2            = match Assign rest1
		(result2, rest3) = expression rest2
	in (AssignK result1 result2, rest3)

read_stmt :: [Token] -> (Stmt, [Token])
read_stmt text = let
		rest1           = match Read text
		(result, rest2) = matchId rest1
	in (ReadK result, rest2)

write_stmt :: [Token] -> (Stmt, [Token])
write_stmt text = let
		rest1           = match Write text
		(result, rest2) = expression rest1
	in (WriteK result, rest2)

expression :: [Token] -> (Exp, [Token])
expression text =
	let (result, rest) = simple_exp text
	in  expression_recursive result rest

expression_recursive :: Exp -> [Token] -> (Exp, [Token])
expression_recursive lh text = let
		rest1             = match Lt text
		(result,  rest2 ) = simple_exp rest1
		rest1'            = match Equal text
		(result', rest2') = simple_exp rest1'
	in case text of
		(Lt   :_) -> expression_recursive (OpK Lt    lh result ) rest2
		(Equal:_) -> expression_recursive (OpK Equal lh result') rest2'
		otherwise -> (lh, text)

simple_exp :: [Token] -> (Exp, [Token])
simple_exp text =
	let (result, rest) = term text
	in  simple_exp_recursive result rest

simple_exp_recursive :: Exp -> [Token] -> (Exp, [Token])
simple_exp_recursive lh text = let
		rest1             = match Plus text
		(result, rest2)   = term rest1
		rest1'            = match Minus text
		(result', rest2') = term rest1'
	in case text of
		(Plus :_) -> simple_exp_recursive (OpK Plus  lh result ) rest2
		(Minus:_) -> simple_exp_recursive (OpK Minus lh result') rest2'
		otherwise -> (lh, text)

term :: [Token] -> (Exp, [Token])
term text = 
	let (result, rest) = factor text
	in  term_recursive result rest

term_recursive :: Exp -> [Token] -> (Exp, [Token])
term_recursive lh text = let
		rest1             = match Times text
		(result,  rest2 ) = factor rest1
		rest1'            = match Over text
		(result', rest2') = factor rest1'
	in case text of
		(Times:_) -> term_recursive (OpK Times lh result ) rest2
		(Over :_) -> term_recursive (OpK Over  lh result') rest2'
		otherwise -> (lh, text)


factor :: [Token] -> (Exp, [Token])
factor text = let
		rest1                = match Lparen text
		(result1,   rest2)   = expression rest1
		rest3                = match Rparen rest2
		(result1',  rest1')  = matchId text
		(result1'', rest1'') = matchNum text
	in case text of
		(Lparen:_) -> (result1, rest3)
		(Id _  :_) -> (IdK result1', rest1')
		(Num _ :_) -> (ConstK result1'', rest1'')
		otherwise  -> error "Parse Error: expected \"(\" or number or identifier"


match :: Token -> [Token] -> [Token]
match y (x:rest)
	| y==x = rest
	| otherwise = error ("Parse Error: expected " ++ show y ++ ", but got " ++ show x)
match y _ = error ("Parse Error: expected " ++ show y)

matchId :: [Token] -> (String, [Token])
matchId (Id name:rest) = (name, rest)
matchId (x:rest) = error ("Parse Error: expected identifier, but got " ++ show x)
matchId _ = error "Parse Error: expected identifier"

matchNum :: [Token] -> (Int, [Token])
matchNum (Num value:rest) = (value, rest)
matchNum (x:rest) = error ("Parse Error: expected number, but got " ++ show x)
matchNum _ = error "Parse Error: expected number"


