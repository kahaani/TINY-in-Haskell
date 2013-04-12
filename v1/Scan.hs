module Scan (
	Token(..),
	scan
) where


data Token = Endfile | Error String
	| If | Then | Else | End | Repeat | Until | Read | Write
	| Id String | Num Int
	| Assign | Equal | Lt | Plus | Minus | Times | Over | Lparen | Rparen | Semi
	deriving (Show, Eq)

data State = Start | InAssign | InComment | InNum | InId


scan :: String -> [Token]
scan contents = let (nextToken, restText) = getToken (Start, contents, [])
	in case nextToken of
		Endfile -> [nextToken]
		otherwise -> nextToken : scan restText


getToken :: (State,String,String) -> (Token,String)

getToken (Start, [], _) = (Endfile, [])
getToken (Start, x:rest, _)
	| x == ' '   = getToken (Start, rest, [])
	| x == '\t'  = getToken (Start, rest, [])
	| x == '\n'  = getToken (Start, rest, [])
	| isdigit(x) = getToken (InNum, rest, [x])
	| isalpha(x) = getToken (InId,  rest, [x])
	| x == ':'   = getToken (InAssign,  rest, [x])
	| x == '{'   = getToken (InComment, rest, [x])
	| x == '='   = (Equal, rest)
	| x == '<'   = (Lt, rest)
	| x == '+'   = (Plus, rest)
	| x == '-'   = (Minus, rest)
	| x == '*'   = (Times, rest)
	| x == '/'   = (Over, rest)
	| x == '('   = (Lparen, rest)
	| x == ')'   = (Rparen, rest)
	| x == ';'   = (Semi, rest)

getToken (InComment, x:rest, current)
	| x == '}'   = getToken (Start, rest, [])
	| otherwise  = getToken (InComment, rest, x:current)

getToken (InAssign,  x:rest, _)
	| x == '='   = (Assign, rest)

getToken (InNum, x:rest, current)
	| isdigit(x) = getToken (InNum, rest, x:current)
getToken (InNum, text, current)
	= (Num . read . reverse $ current, text)

getToken (InId, x:rest, current)
	| isalpha(x) = getToken (InId, rest, x:current)
getToken (InId, text, current)
	= (reservedLookup . Id . reverse $ current, text)

-- Alternative Error Handle
--getToken (_, rest, current)
--	= (Error . reverse $ current, rest)

getToken (_, _, current)
	= error $ "Scan Error: " ++ show (reverse current)


reservedLookup :: Token -> Token
reservedLookup (Id "if")     = If
reservedLookup (Id "then")   = Then
reservedLookup (Id "else")   = Else
reservedLookup (Id "end")    = End
reservedLookup (Id "repeat") = Repeat
reservedLookup (Id "until")  = Until
reservedLookup (Id "read")   = Read
reservedLookup (Id "write")  = Write
reservedLookup x = x

isdigit :: Char -> Bool
isdigit x = x >= '0' && x <= '9'

isalpha :: Char -> Bool
isalpha x = x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z'


