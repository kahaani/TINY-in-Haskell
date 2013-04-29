module CodeGen (
	codeGen
) where

import Control.Monad
import Data.Monoid
import qualified Data.Map as Map

import Scan (Token(..))
import Parse (Stmt(..), Exp(..))
import BuildSymtab (Variable, Location, Symtab)

------------------------------

type CodeLine = String
type CodeFile = [CodeLine]

newtype GenM a = GenM { runGen :: (a, CodeFile) }

instance Monad GenM where
	return x = GenM (x, mempty)
	(GenM (x,v)) >>= f = let (GenM (y, v')) = f x in GenM (y, v `mappend` v')

tell :: CodeLine -> GenM ()
tell codeline = GenM ((), [codeline])

------------------------------

type EmitLoc     = Int
type HighEmitLoc = Int
type TmpOffset   = Int
type EmitState   = (EmitLoc, HighEmitLoc, TmpOffset)

emitComment :: String -> GenM ()
emitComment str = do
	tell $ "* " ++ str

emitRO :: String -> Int -> Int -> Int -> String -> EmitState -> GenM EmitState
emitRO op r s t comm (emitLoc, highEmitLoc, tmpOffset) = do
	let newEmitLoc = emitLoc + 1
	let newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
	tell $ show emitLoc
		++ ": " ++ op
		++ ' ' : show r
		++ ',' : show s
		++ ',' : show t
		++ '\t' : comm
	return (newEmitLoc, newHighEmitLoc, tmpOffset)

emitRM :: String -> Int -> Int -> Int -> String -> EmitState -> GenM EmitState
emitRM op r d s comm (emitLoc, highEmitLoc, tmpOffset) = do
	let newEmitLoc = emitLoc + 1
	let newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
	tell $ show emitLoc
		++ ": " ++ op
		++ ' ' : show r
		++ ',' : show d
		++ '(' : show s
		++ ")\t" ++ comm
	return (newEmitLoc, newHighEmitLoc, tmpOffset)

emitRM_Abs :: String -> Int -> Int -> String -> EmitState -> GenM EmitState
emitRM_Abs op r a comm (emitLoc, highEmitLoc, tmpOffset) = do
	let newEmitLoc = emitLoc + 1
	let newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
	tell $ show emitLoc
		++ ": " ++ op
		++ ' ' : show r
		++ ',' : show (a-emitLoc-1)
		++ '(' : show pc
		++ ")\t" ++ comm
	return (newEmitLoc, newHighEmitLoc, tmpOffset)

emitSkip :: Int -> EmitState -> GenM (EmitState, Int)
emitSkip howMany (emitLoc, highEmitLoc, tmpOffset) = do
	let newEmitLoc = emitLoc + howMany
	let newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
	return ((newEmitLoc, newHighEmitLoc, tmpOffset), emitLoc)

emitBackup :: Int -> EmitState -> GenM EmitState
emitBackup loc (emitLoc, highEmitLoc, tmpOffset) = do
	if   loc > highEmitLoc
	then error "BUG in emitBackup"
	else return (loc,highEmitLoc, tmpOffset)

emitRestore :: EmitState -> GenM EmitState
emitRestore (emitLoc, highEmitLoc, tmpOffset) = do
	return (highEmitLoc, highEmitLoc, tmpOffset)

------------------------------

ac = 0
ac1 = 1
gp = 5
mp = 6
pc = 7

genStmts :: [Stmt] -> Symtab -> EmitState -> GenM EmitState
genStmts stmts st state = foldM (\acc x -> genStmt x st acc) state stmts

genStmt :: Stmt -> Symtab -> EmitState -> GenM EmitState
genStmt (If2K exp stmts) st state = genStmt (If3K exp stmts []) st state
genStmt (If3K exp stmts1 stmts2) st state = do
	emitComment "-> if"
	state1 <- genExp exp st state
	(state2,saveLoc1) <- emitSkip 1 state1
	emitComment "if: jump to else belongs here"
	state3 <- genStmts stmts1 st state2
	(state4,saveLoc2) <- emitSkip 1 state3
	emitComment "if: jump to end belongs here"
	(state5,currLoc1) <- emitSkip 0 state4
	state6 <- emitBackup saveLoc1 state5
	state7 <- emitRM_Abs "JEQ" ac currLoc1 "if: jmp to else" state6
	state8 <- emitRestore state7
	state9 <- genStmts stmts2 st state8
	(state10,currLoc2) <- emitSkip 0 state9
	state11 <- emitBackup saveLoc2 state10
	state12 <- emitRM_Abs "LDA" pc currLoc2 "jmp to end" state11
	state13 <- emitRestore state12
	emitComment "<- if"
	return state13
genStmt (RepeatK stmts exp) st state = do
	emitComment "-> repeat"
	(state1,saveLoc1) <- emitSkip 0 state
	emitComment "repeat: jump after body comes back here"
	state2 <- genStmts stmts st state1
	state3 <- genExp exp st state2
	state4 <- emitRM_Abs "JEQ" ac saveLoc1 "repeat: jmp back to body" state3
	emitComment "<- repeat"
	return state4
genStmt (AssignK iden exp) st state = do
	emitComment "-> assign"
	state1 <- genExp exp st state
	let loc = stLookup st iden
	state2 <- emitRM "ST" ac loc gp "assign: store value" state1
	emitComment "<- assign"
	return state2
genStmt (ReadK iden) st state = do
	state1 <- emitRO "IN" ac 0 0 "read integer value" state
	let loc = stLookup st iden
	state2 <- emitRM "ST" ac loc gp "read: store value" state1
	return state2
genStmt (WriteK exp) st state = do
	state1 <- genExp exp st state
	state2 <- emitRO "OUT" ac 0 0 "write ac" state1
	return state2

genExp :: Exp -> Symtab -> EmitState -> GenM EmitState
genExp (ConstK value) st state = do
	emitComment "-> Const"
	state1 <- emitRM "LDC" ac value 0 "load const" state
	emitComment "<- Const"
	return state1
genExp (IdK iden) st state = do
	emitComment "-> Id"
	let loc = stLookup st iden
	state1 <- emitRM "LD" ac loc gp "load id value" state
	emitComment "<- Id"
	return state1
genExp (OpK op exp1 exp2) st state = do
	emitComment "-> Op"
	state1@(_, _, tmpOffset1) <- genExp exp1 st state
	state2@(el2, hel2, _)     <- emitRM "ST" ac tmpOffset1 mp "op: push left" state1
	let state3 = (el2, hel2, tmpOffset1 - 1)
	state4@(_, _, tmpOffset4) <- genExp exp2 st state3
	state5@(el5, hel5, _)     <- emitRM "LD" ac1 (tmpOffset4 + 1) mp "op: load left" state4
	let state6 = (el5, hel5, tmpOffset4 + 1)
	state7 <- case op of
		Plus  -> emitRO "ADD" ac ac1 ac "op +" state6
		Minus -> emitRO "SUB" ac ac1 ac "op -" state6
		Times -> emitRO "MUL" ac ac1 ac "op *" state6
		Over  -> emitRO "DIV" ac ac1 ac "op /" state6
		Lt    -> do
			state61 <- emitRO "SUB" ac ac1 ac "op <"              state6
			state62 <- emitRM "JLT" ac 2   pc "br if true"        state61
			state63 <- emitRM "LDC" ac 0   ac "false case"        state62
			state64 <- emitRM "LDA" pc 1   pc "unconditional jmp" state63
			state65 <- emitRM "LDC" ac 1   ac "true case"         state64
			return state65
		Equal -> do
			state61 <- emitRO "SUB" ac ac1 ac "op =="             state6
			state62 <- emitRM "JEQ" ac 2   pc "br if true"        state61
			state63 <- emitRM "LDC" ac 0   ac "false case"        state62
			state64 <- emitRM "LDA" pc 1   pc "unconditional jmp" state63
			state65 <- emitRM "LDC" ac 1   ac "true case"         state64
			return state65
		otherwise -> do
			emitComment "BUG: Unknown operator"
			return state6
	emitComment "<- Op"
	return state7

getEnv :: [Stmt] -> Symtab -> EmitState -> GenM ()
getEnv stmts st state = do
	emitComment "TINY Compilation to TM Code"
	emitComment "Standard prelude:"
	state1 <- emitRM "LD" mp 0 ac "load maxaddress from location 0" state
	state2 <- emitRM "ST" ac 0 ac "clear location 0" state1
	emitComment "End of standard prelude."
	state3 <- genStmts stmts st state2
	emitComment "End of execution."
	state4 <- emitRO "HALT" 0 0 0 "" state3
	return ()

------------------------------

stLookup :: Symtab -> Variable -> Location
stLookup st v = case Map.lookup v st of
	Just x  -> x
	Nothing -> error "BUG in buildSymtab"

codeGen :: ([Stmt], Symtab) -> CodeFile
codeGen (stmts, st) = snd . runGen $ getEnv stmts st (0,0,0)

