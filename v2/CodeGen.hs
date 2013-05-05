module CodeGen (
	codeGen
) where

import Control.Monad
import qualified Data.Map as Map

import Scan (Token(..))
import Parse (Stmt(..), Exp(..))
import BuildSymtab (Variable, Location, Symtab)

------------------------------

type CodeLine = String
type CodeFile = [CodeLine] -- difference list?
type State = (EmitState, CodeFile)

newtype StateM a = StateM {
		runState :: State -> (a, State)
	}

instance Monad StateM where
	return x = StateM $ \s -> (x,s)
	(StateM h) >>= f = StateM $ \s-> let
			(res, newstate) = h s
			(StateM g) = f res
		in g newstate

------------------------------

type EmitLoc     = Int
type HighEmitLoc = Int
type TmpOffset   = Int
type EmitState   = (EmitLoc, HighEmitLoc, TmpOffset)

emitCommentM :: String -> StateM ()
emitCommentM str = StateM $ emitComment str

emitComment :: String -> State -> ((), State)
emitComment str (emitState, codeFile) = let
		codeLine = "* " ++ str
		newCodeFile = codeFile ++ [codeLine]
		newState = (emitState, newCodeFile)
	in ((), newState)

emitROM :: String -> Int -> Int -> Int -> String -> StateM ()
emitROM op r s t comm = StateM $ emitRO op r s t comm

emitRO :: String -> Int -> Int -> Int -> String -> State -> ((), State)
emitRO op r s t comm ((emitLoc, highEmitLoc, tmpOffset), codeFile) = let
		newEmitLoc = emitLoc + 1
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		codeLine = show emitLoc
			++ ": " ++ op
			++ ' ' : show r
			++ ',' : show s
			++ ',' : show t
			++ '\t' : comm
		newCodeFile = codeFile ++ [codeLine]
		newState = ((newEmitLoc, newHighEmitLoc, tmpOffset), newCodeFile)
	in ((), newState)

emitRMM :: String -> Int -> Int -> Int -> String -> StateM ()
emitRMM op r d s comm = StateM $ emitRM op r d s comm

emitRM :: String -> Int -> Int -> Int -> String -> State -> ((), State)
emitRM op r d s comm ((emitLoc, highEmitLoc, tmpOffset), codeFile) = let
		newEmitLoc = emitLoc + 1
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		codeLine = show emitLoc
			++ ": " ++ op
			++ ' ' : show r
			++ ',' : show d
			++ '(' : show s
			++ ")\t" ++ comm
		newCodeFile = codeFile ++ [codeLine]
		newState = ((newEmitLoc, newHighEmitLoc, tmpOffset), newCodeFile)
	in ((), newState)

emitRM_AbsM :: String -> Int -> Int -> String -> StateM ()
emitRM_AbsM op r a comm = StateM $ emitRM_Abs op r a comm

emitRM_Abs :: String -> Int -> Int -> String -> State -> ((), State)
emitRM_Abs op r a comm ((emitLoc, highEmitLoc, tmpOffset), codeFile) = let
		newEmitLoc = emitLoc + 1
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		codeLine = show emitLoc
			++ ": " ++ op
			++ ' ' : show r
			++ ',' : show (a-emitLoc-1)
			++ '(' : show pc
			++ ")\t" ++ comm
		newCodeFile = codeFile ++ [codeLine]
		newState = ((newEmitLoc, newHighEmitLoc, tmpOffset), newCodeFile)
	in ((), newState)

emitSkipM :: Int -> StateM EmitLoc
emitSkipM howMany = StateM $ emitSkip howMany

emitSkip :: Int -> State -> (EmitLoc, State)
emitSkip howMany ((emitLoc, highEmitLoc, tmpOffset), codeFile) = let
		newEmitLoc = emitLoc + howMany
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		newState = ((newEmitLoc, newHighEmitLoc, tmpOffset), codeFile)
	in (emitLoc, newState)

emitBackupM :: Int -> StateM ()
emitBackupM loc = StateM $ emitBackup loc

emitBackup :: Int -> State -> ((), State)
emitBackup loc ((emitLoc, highEmitLoc, tmpOffset), codeFile) =
	if   loc > highEmitLoc
	then error "BUG in emitBackup"
	else ((), ((loc, highEmitLoc, tmpOffset), codeFile))

emitRestoreM :: StateM ()
emitRestoreM = StateM $ emitRestore

emitRestore :: State -> ((), State)
emitRestore ((emitLoc, highEmitLoc, tmpOffset), codeFile) =
	((), ((highEmitLoc, highEmitLoc, tmpOffset), codeFile))

------------------------------

getTmpOffsetM :: StateM TmpOffset
getTmpOffsetM = StateM $ getTmpOffset

getTmpOffset :: State -> (TmpOffset, State)
getTmpOffset state@((emitLoc, highEmitLoc, tmpOffset), codeFile) =
	(tmpOffset, state)

incTmpOffsetM :: StateM ()
incTmpOffsetM = StateM $ incTmpOffset

incTmpOffset :: State -> ((), State)
incTmpOffset ((emitLoc, highEmitLoc, tmpOffset), codeFile) =
	((), ((emitLoc, highEmitLoc, tmpOffset+1), codeFile))

decTmpOffsetM :: StateM ()
decTmpOffsetM = StateM $ decTmpOffset

decTmpOffset :: State -> ((), State)
decTmpOffset ((emitLoc, highEmitLoc, tmpOffset), codeFile) =
	((), ((emitLoc, highEmitLoc, tmpOffset-1), codeFile))

------------------------------

ac = 0
ac1 = 1
gp = 5
mp = 6
pc = 7

-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
genStmtsM :: [Stmt] -> Symtab -> StateM ()
genStmtsM stmts symtab = mapM_ (\x -> genStmtM x symtab) stmts

genStmtM :: Stmt -> Symtab -> StateM ()
genStmtM (If2K exp stmts) symtab = genStmtM (If3K exp stmts []) symtab
genStmtM (If3K exp stmts1 stmts2) symtab = do
	emitCommentM "-> if"
	genExpM exp symtab
	saveLoc1 <- emitSkipM 1
	emitCommentM "if: jump to else belongs here"
	genStmtsM stmts1 symtab
	saveLoc2 <- emitSkipM 1
	emitCommentM "if: jump to end belongs here"
	currLoc1 <- emitSkipM 0
	emitBackupM saveLoc1
	emitRM_AbsM "JEQ" ac currLoc1 "if: jmp to else"
	emitRestoreM
	genStmtsM stmts2 symtab
	currLoc2 <- emitSkipM 0
	emitBackupM saveLoc2
	emitRM_AbsM "LDA" pc currLoc2 "jmp to end"
	emitRestoreM
	emitCommentM "<- if"
genStmtM (RepeatK stmts exp) symtab = do
	emitCommentM "-> repeat"
	saveLoc1 <- emitSkipM 0
	emitCommentM "repeat: jump after body comes back here"
	genStmtsM stmts symtab
	genExpM exp symtab
	emitRM_AbsM "JEQ" ac saveLoc1 "repeat: jmp back to body"
	emitCommentM "<- repeat"
genStmtM (AssignK iden exp) symtab = do
	emitCommentM "-> assign"
	genExpM exp symtab
	let loc = stLookup symtab iden
	emitRMM "ST" ac loc gp "assign: store value"
	emitCommentM "<- assign"
genStmtM (ReadK iden) symtab = do
	emitROM "IN" ac 0 0 "read integer value"
	let loc = stLookup symtab iden
	emitRMM "ST" ac loc gp "read: store value"
genStmtM (WriteK exp) symtab = do
	genExpM exp symtab
	emitROM "OUT" ac 0 0 "write ac"

genExpM :: Exp -> Symtab -> StateM ()
genExpM (ConstK value) symtab = do
	emitCommentM "-> Const"
	emitRMM "LDC" ac value 0 "load const"
	emitCommentM "<- Const"
genExpM (IdK iden) symtab = do
	emitCommentM "-> Id"
	let loc = stLookup symtab iden
	emitRMM "LD" ac loc gp "load id value"
	emitCommentM "<- Id"
genExpM (OpK op exp1 exp2) symtab = do
	emitCommentM "-> Op"
	genExpM exp1 symtab
	tmpOffset1 <- getTmpOffsetM
	emitRMM "ST" ac tmpOffset1 mp "op: push left"
	decTmpOffsetM
	genExpM exp2 symtab
	incTmpOffsetM
	tmpOffset2 <- getTmpOffsetM
	emitRMM "LD" ac1 tmpOffset2 mp "op: load left"
	case op of
		Plus  -> emitROM "ADD" ac ac1 ac "op +"
		Minus -> emitROM "SUB" ac ac1 ac "op -"
		Times -> emitROM "MUL" ac ac1 ac "op *"
		Over  -> emitROM "DIV" ac ac1 ac "op /"
		Lt    -> do
			emitROM "SUB" ac ac1 ac "op <"
			emitRMM "JLT" ac 2   pc "br if true"
			emitRMM "LDC" ac 0   ac "false case"
			emitRMM "LDA" pc 1   pc "unconditional jmp"
			emitRMM "LDC" ac 1   ac "true case"
		Equal -> do
			emitROM "SUB" ac ac1 ac "op =="
			emitRMM "JEQ" ac 2   pc "br if true"
			emitRMM "LDC" ac 0   ac "false case"
			emitRMM "LDA" pc 1   pc "unconditional jmp"
			emitRMM "LDC" ac 1   ac "true case"
		otherwise -> error "BUG: Unknown operator"
	emitCommentM "<- Op"

getEnv :: [Stmt] -> Symtab -> StateM ()
getEnv stmts symtab = do
	emitCommentM "TINY Compilation to TM Code"
	emitCommentM "Standard prelude:"
	emitRMM "LD" mp 0 ac "load maxaddress from location 0"
	emitRMM "ST" ac 0 ac "clear location 0"
	emitCommentM "End of standard prelude."
	genStmtsM stmts symtab
	emitCommentM "End of execution."
	emitROM "HALT" 0 0 0 ""

------------------------------

stLookup :: Symtab -> Variable -> Location
stLookup st v = case Map.lookup v st of
	Just x  -> x
	Nothing -> error "BUG in buildSymtab"

codeGen :: ([Stmt], Symtab) -> CodeFile
codeGen (stmts, symtab) = snd . snd . runState (getEnv stmts symtab) $ ((0,0,0),[])

