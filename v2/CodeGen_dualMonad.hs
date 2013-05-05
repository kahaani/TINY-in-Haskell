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

newtype WriterM a = WriterM { runWriter :: (a, CodeFile) }

instance Monad WriterM where
	return x = WriterM (x, mempty)
	(WriterM (x,v)) >>= f = let (WriterM (y, v')) = f x in WriterM (y, v `mappend` v')

tell :: CodeLine -> WriterM ()
tell codeline = WriterM ((), [codeline])

------------------------------

--type State = (EmitState, Symtab)

newtype StateM a = StateM {
		runState :: EmitState -> (a, EmitState)
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

emitCommentM :: String -> StateM (WriterM ())
emitCommentM str = StateM $ emitComment str

emitComment :: String -> EmitState -> (WriterM (), EmitState)
emitComment str state = (tell $ "* " ++ str, state)

emitROM :: String -> Int -> Int -> Int -> String -> StateM (WriterM ())
emitROM op r s t comm = StateM $ emitRO op r s t comm

emitRO :: String -> Int -> Int -> Int -> String -> EmitState -> (WriterM (), EmitState)
emitRO op r s t comm (emitLoc, highEmitLoc, tmpOffset) = let
		newEmitLoc = emitLoc + 1
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		writer = tell $ show emitLoc
			++ ": " ++ op
			++ ' ' : show r
			++ ',' : show s
			++ ',' : show t
			++ '\t' : comm
		newstate = (newEmitLoc, newHighEmitLoc, tmpOffset)
	in (writer, newstate)

emitRMM :: String -> Int -> Int -> Int -> String -> StateM (WriterM ())
emitRMM op r d s comm = StateM $ emitRM op r d s comm

emitRM :: String -> Int -> Int -> Int -> String -> EmitState -> (WriterM (), EmitState)
emitRM op r d s comm (emitLoc, highEmitLoc, tmpOffset) = let
		newEmitLoc = emitLoc + 1
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		writer = tell $ show emitLoc
			++ ": " ++ op
			++ ' ' : show r
			++ ',' : show d
			++ '(' : show s
			++ ")\t" ++ comm
		newstate = (newEmitLoc, newHighEmitLoc, tmpOffset)
	in (writer, newstate)

emitRM_AbsM :: String -> Int -> Int -> String -> StateM (WriterM ())
emitRM_AbsM op r a comm = StateM $ emitRM_Abs op r a comm

emitRM_Abs :: String -> Int -> Int -> String -> EmitState -> (WriterM (), EmitState)
emitRM_Abs op r a comm (emitLoc, highEmitLoc, tmpOffset) = let
		newEmitLoc = emitLoc + 1
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		writer = tell $ show emitLoc
			++ ": " ++ op
			++ ' ' : show r
			++ ',' : show (a-emitLoc-1)
			++ '(' : show pc
			++ ")\t" ++ comm
		newstate = (newEmitLoc, newHighEmitLoc, tmpOffset)
	in (writer, newstate)

emitSkipM :: Int -> StateM Int
emitSkipM howMany = StateM $ emitSkip howMany

emitSkip :: Int -> EmitState -> (Int, EmitState)
emitSkip howMany (emitLoc, highEmitLoc, tmpOffset) = let
		newEmitLoc = emitLoc + howMany
		newHighEmitLoc = if highEmitLoc < newEmitLoc then newEmitLoc else highEmitLoc
		newstate = (newEmitLoc, newHighEmitLoc, tmpOffset)
	in (emitLoc, newstate)

emitBackupM :: Int -> StateM ()
emitBackupM loc = StateM $ emitBackup loc

emitBackup :: Int -> EmitState -> ((), EmitState)
emitBackup loc (emitLoc, highEmitLoc, tmpOffset) =
	if   loc > highEmitLoc
	then error "BUG in emitBackup"
	else ((), (loc, highEmitLoc, tmpOffset))

emitRestoreM :: StateM ()
emitRestoreM = StateM $ emitRestore

emitRestore :: EmitState -> ((), EmitState)
emitRestore (emitLoc, highEmitLoc, tmpOffset) =
	((), (highEmitLoc, highEmitLoc, tmpOffset))

------------------------------

getTmpOffsetM :: StateM TmpOffset
getTmpOffsetM = StateM $ getTmpOffset

getTmpOffset :: EmitState -> (TmpOffset, EmitState)
getTmpOffset (emitLoc, highEmitLoc, tmpOffset) =
	(tmpOffset, (emitLoc, highEmitLoc, tmpOffset))

incTmpOffsetM :: StateM ()
incTmpOffsetM = StateM $ incTmpOffset

incTmpOffset :: EmitState -> ((), EmitState)
incTmpOffset (emitLoc, highEmitLoc, tmpOffset) =
	((), (emitLoc, highEmitLoc, tmpOffset+1))

decTmpOffsetM :: StateM ()
decTmpOffsetM = StateM $ decTmpOffset

decTmpOffset :: EmitState -> ((), EmitState)
decTmpOffset (emitLoc, highEmitLoc, tmpOffset) =
	((), (emitLoc, highEmitLoc, tmpOffset-1))

------------------------------

ac = 0
ac1 = 1
gp = 5
mp = 6
pc = 7

-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
genStmtsM :: [Stmt] -> Symtab -> StateM (WriterM ())
genStmtsM stmts symtab = do
	res <- mapM (\x -> genStmtM x symtab) stmts
	case null res of
		True  -> return $ return ()
		False -> return $ foldr1 (>>) res

genStmtM :: Stmt -> Symtab -> StateM (WriterM ())
genStmtM (If2K exp stmts) symtab = genStmtM (If3K exp stmts []) symtab
genStmtM (If3K exp stmts1 stmts2) symtab = do
	w1 <- emitCommentM "-> if"
	w2 <- genExpM exp symtab
	saveLoc1 <- emitSkipM 1
	w3 <- emitCommentM "if: jump to else belongs here"
	w4 <- genStmtsM stmts1 symtab
	saveLoc2 <- emitSkipM 1
	w5 <- emitCommentM "if: jump to end belongs here"
	currLoc1 <- emitSkipM 0
	emitBackupM saveLoc1
	w6 <- emitRM_AbsM "JEQ" ac currLoc1 "if: jmp to else"
	emitRestoreM
	w7 <- genStmtsM stmts2 symtab
	currLoc2 <- emitSkipM 0
	emitBackupM saveLoc2
	w8 <- emitRM_AbsM "LDA" pc currLoc2 "jmp to end"
	emitRestoreM
	w9 <- emitCommentM "<- if"
	return $ w1 >> w2 >> w3 >> w4 >> w5 >> w6 >> w7 >> w8 >> w9
genStmtM (RepeatK stmts exp) symtab = do
	w1 <- emitCommentM "-> repeat"
	saveLoc1 <- emitSkipM 0
	w2 <- emitCommentM "repeat: jump after body comes back here"
	w3 <- genStmtsM stmts symtab
	w4 <- genExpM exp symtab
	w5 <- emitRM_AbsM "JEQ" ac saveLoc1 "repeat: jmp back to body"
	w6 <- emitCommentM "<- repeat"
	return $ w1 >> w2 >> w3 >> w4 >> w5 >> w6
genStmtM (AssignK iden exp) symtab = do
	w1 <- emitCommentM "-> assign"
	w2 <- genExpM exp symtab
	let loc = stLookup symtab iden
	w3 <- emitRMM "ST" ac loc gp "assign: store value"
	w4 <- emitCommentM "<- assign"
	return $ w1 >> w2 >> w3 >> w4
genStmtM (ReadK iden) symtab = do
	w1 <- emitROM "IN" ac 0 0 "read integer value"
	let loc = stLookup symtab iden
	w2 <- emitRMM "ST" ac loc gp "read: store value"
	return $ w1 >> w2
genStmtM (WriteK exp) symtab = do
	w1 <- genExpM exp symtab
	w2 <- emitROM "OUT" ac 0 0 "write ac"
	return $ w1 >> w2

genExpM :: Exp -> Symtab -> StateM (WriterM ())
genExpM (ConstK value) symtab = do
	w1 <- emitCommentM "-> Const"
	w2 <- emitRMM "LDC" ac value 0 "load const"
	w3 <- emitCommentM "<- Const"
	return $ w1 >> w2 >> w3
genExpM (IdK iden) symtab = do
	w1 <- emitCommentM "-> Id"
	let loc = stLookup symtab iden
	w2 <- emitRMM "LD" ac loc gp "load id value"
	w3 <- emitCommentM "<- Id"
	return $ w1 >> w2 >> w3
genExpM (OpK op exp1 exp2) symtab = do
	w1 <- emitCommentM "-> Op"
	w2 <- genExpM exp1 symtab
	tmpOffset1 <- getTmpOffsetM
	w3 <- emitRMM "ST" ac tmpOffset1 mp "op: push left"
	decTmpOffsetM
	w4 <- genExpM exp2 symtab
	incTmpOffsetM
	tmpOffset2 <- getTmpOffsetM
	w5 <- emitRMM "LD" ac1 tmpOffset2 mp "op: load left"
	w6 <- case op of
		Plus  -> emitROM "ADD" ac ac1 ac "op +"
		Minus -> emitROM "SUB" ac ac1 ac "op -"
		Times -> emitROM "MUL" ac ac1 ac "op *"
		Over  -> emitROM "DIV" ac ac1 ac "op /"
		Lt    -> do
			w61 <- emitROM "SUB" ac ac1 ac "op <"
			w62 <- emitRMM "JLT" ac 2   pc "br if true"
			w63 <- emitRMM "LDC" ac 0   ac "false case"
			w64 <- emitRMM "LDA" pc 1   pc "unconditional jmp"
			w65 <- emitRMM "LDC" ac 1   ac "true case"
			return $ w61 >> w62 >> w63 >> w64 >> w65
		Equal -> do
			w61 <- emitROM "SUB" ac ac1 ac "op =="
			w62 <- emitRMM "JEQ" ac 2   pc "br if true"
			w63 <- emitRMM "LDC" ac 0   ac "false case"
			w64 <- emitRMM "LDA" pc 1   pc "unconditional jmp"
			w65 <- emitRMM "LDC" ac 1   ac "true case"
			return $ w61 >> w62 >> w63 >> w64 >> w65
		otherwise -> emitCommentM "BUG: Unknown operator"
	w7 <- emitCommentM "<- Op"
	return $ w1 >> w2 >> w3 >> w4 >> w5 >> w6 >> w7

getEnv :: [Stmt] -> Symtab -> StateM (WriterM ())
getEnv stmts symtab = do
	w1 <- emitCommentM "TINY Compilation to TM Code"
	w2 <- emitCommentM "Standard prelude:"
	w3 <- emitRMM "LD" mp 0 ac "load maxaddress from location 0"
	w4 <- emitRMM "ST" ac 0 ac "clear location 0"
	w5 <- emitCommentM "End of standard prelude."
	w6 <- genStmtsM stmts symtab
	w7 <- emitCommentM "End of execution."
	w8 <- emitROM "HALT" 0 0 0 ""
	return $ w1 >> w2 >> w3 >> w4 >> w5 >> w6 >> w7 >> w8

------------------------------

stLookup :: Symtab -> Variable -> Location
stLookup st v = case Map.lookup v st of
	Just x  -> x
	Nothing -> error "BUG in buildSymtab"

codeGen :: ([Stmt], Symtab) -> CodeFile
codeGen (stmts, symtab) = snd . runWriter . fst . runState (getEnv stmts symtab) $ (0,0,0)

