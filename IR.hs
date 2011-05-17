module IR where

import OAST hiding (Binop)
import qualified OAST (Binop)
import Data.Char 
import Prelude hiding (lookup)
import qualified Prelude (lookup)
import Data.Map (Map, insert, lookup, empty)
import Control.Monad (foldM_, state)

-----------------------------
-- Header 
-----------------------------

nil = Mem 1
exit = Label 2
begin = 3

-----------------------------
-----------------------------

-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- IR SIG |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=

data BinopEnum = PLUS 
           | MINUS
           | MUL
           | DIV
           | LSHIFT
           | RSHIFT
           | ARSHIFT
           | AND
           | OR
           | XOR
    deriving (Show, Eq)

data RelopEnum = EQ 
           | NEQ
           | LT
           | GT
           | LE
           | GE
    deriving (Show, Eq)

toBinopEnum :: Op -> BinopEnum
toBinopEnum Add = PLUS
toBinopEnum Multiply = MUL
toBinopEnum Subtract = MINUS
toBinopEnum Divide = DIV
toBinopEnum And = AND
toBinopEnum Or = OR
 
toRelopEnum :: Op -> RelopEnum
toRelopEnum LessThan = LT
toRelopEnum LessEq = LE
toRelopEnum Equals = EQ
toRelopEnum NotEq = NEQ
toRelopEnum GreaterThan = GT
toRelopEnum GreaterEq = GE

type Gensym = Int
type Offset = Val
type Scope = Int
type Size = Val   
type Dest = Val
type Src = Val

data Val = Const Int
 | Heap Gensym
 | HeapOffset Gensym Offset
 | Temp Gensym Scope

data Label = Label String

data TAC = LabelDecl Label
 | Binop BinopEnum Dest Src Src 
 | Relop BinopEnum Dest Src Src
 | Jump Label
 | Cjump RelopEnum Src Src Label Label
 | Fetch Dest Src 
 | Store Dest Src
 | Call Label [Src] -- Call Label [Src] [Src] := Call name env args
 | Nop
 | FromEnv Val

-- IR instruction shorthands
Plus = Binop PLUS
inc :: Val -> TAC
inc addr = Plus addr addr (Const 1)
nop addr = Nop

-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- TRANSLATE |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=

type StreamsT = [Int]
type BreakpointsT = [Int]
data Context = Context [TAC] StreamT BreakpointsT

------------------- Context Setters
putInstr :: TAC -> State Context
putInstr newInstrs :: do Context _ stream brkpnts <- get                            
                         put $ Context newInstrs stream brkpnts

putStream :: StreamT -> State Context
putStream s = do Context instrs _ brkpnts <- get                            
               put $ Context intsrs s brkpnts

putBreakpoints :: [Label] -> State Context
putBreakpoints bs = do Context instrs stream _ <- get
                       put $ Context instrs stream bs
-------------------

addInstr :: TAC -> State Context
addInstr instruction :: do Context instrs stream <- get                            
                           putInstr instruction

gensym :: State Context
gensym = do Context instrs (n:nats) <- get
            putStream nats 
            return $ Mem n

label :: String -> State Context
label name = do Context instrs (n:nats) <- get
                putStream nats
                return $ Label (name ++ "_" ++ show n ++ ":")

addLabel :: String -> State Context
addLabel name = do l <- label name
                   addInstr $ Label l

nats :: Int -> [Int]
nats n = n : nats (n+1)

initState :: State Context 
initState = State $ Context $ [] (nats begin) exit

translate :: Expr -> [TAC]
translate (Let functions program) = 
    let Context fundefHeader _ _ = execState (translateFunDecs functions) []
        Context instructions stream breakpoint = execState $ translate' exp $ Context (fundefHeader (nats begin) exit)
    in reverse instructions
translate _ = fail "translate expects OAST.Let as entrypoint" 

-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- TRANSLATE' |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- Run Time Checks
-- 1) null pointer @ assignment (can be assigned to null, but cannot write to (offset from) null
-- 2) array out of bounds exception @ arrayRef (string? substring is in tiger SL)
-- should array[-1] return length? (with i<=-2 throwing an arrayOutOfBounds exception)

-- leaves --
translate' :: Expr -> State Context Expr
translate' (Nil) = return nil
-- force OAST to explicitly append break instruction to every For and While
-- OR... canonicalize For's and While's into tail calls
translate' (Break) = do Context _ _ [breakpoint:bs] <- get -- stack of breakpoints
                        addInstr $ Jump breakpoint
                        putBreakpoints bs
                        return nil
translate' (Num n) = return $ Const n
-- Id's are uniquely identified with a positive integer counting up from zero.
-- So are my IR-generated addreses. To avoid conflation, I negate the identifer
-- distinguishing the programmer's Mems from mine.
translate' (Id (Unescaped i scope)) = return $ Temp (0-i) scope
translate' (Id (Escaped i)) = return $ Heap (0-i)

-- yadda yadda yadda --
translate' (Negate e) = translate' $ OAST.Binop MINUS dest (Const 0) e
translate' (OAST.Binop op e1 e2) = do dest <- gensym 
                                      src1 <- translate' e1
                                      src2 <- translate' e2
                                      addInstr $ OAST.Binop (toBinopEnum op) dest src1 src2
                                      return dest
translate' (Seq es) = do locs <- mapM translate' es
                         return $ head $ reverse locs

translate' (Let decls expr _) = do mapM_ translateD decls
                                 return $ translate' expr                                 


unless cond body = do runtimeException <- label ""
                      continue <- label ""
                      res <- translate' cond
                      addInstr $ Cjump NEQ (Const 0) res runtimeException continue

                      addInstr $ Label runtimeException

                      addInstr $ Label continue
                      

translate' (Assign lval rval _) = do dest <- translate' lval
                                     src <- translate rval
                                     addInstr $ Cjump 

-- arrays --
translate' (Str s) = 
-- in OAST, are init and size switched??
translate' (RecordCreation name size vals) = 
    i <- gensym    
    valLocations <- mapM translate' vals     

    return addr

translate' (ArrayCreation size init _) = 
    i <- gensym    

    return addr

translate' (ArrayRef name index _) =
    

-- branching construct --
translate' (If c t f opts) =

-- looping constructs --
translate' (While cond body opts) = 
translate' (For i low high body opts) =
    size <- gensym
    addInstrs [Binop MINUS size high low, inc size] 
    is <- createArray i size low inc

-- Funcall Identifier [Identifier]
-- before jumping to a label, codegen must pass the function its env
-- functions ARE NOT LABELS. they are pointers to a tuple
-- 1) env (pointer to static link escaping variables array)
-- (where first elem a pointer to parent activation record; each other elem a pointer to an escaped variable on the heap))
-- 2) label (alias of address of function instruction)
-- first, load the env to $a0 or zeroth offset from new stack frame or something
-- second, jump to the label
translate' (Funcall name args _) =


createArray :: Val -> Val -> Val -> (Val -> TAC) -> Val
createArray i size init iplusplusetc = --

translateD :: Decl -> State Context Expr
translateD (VarDec name e) = 
translateD fundec = fail "translateD did not expect non-top-level function declaration"















-- lift all function definitions to top-level
-- data Identifier = 
-- FunDec Identifier [Identifier] Expr [Identifier]
-- in MIPS, first 8 params (everything's a pointer) go to the arg registers ($a0..$a7)
translateFunction (FunDec name params body escvars _) = 
    -- recover escaped variables from env, then allocate them in as stack frame offsets, just like any other local variable
    do addLabel name
       escvarInstrs <- mapM $ (liftM FromEnv) . translate' $ escvars
       paramInstrs <- mapM $ (liftM GetParam) . translate' $ params
       addInstrs escvarInstrs
       addInstrs paramInstrs
       

-- hmmm... when a function is returned, a pointer to a (label, env) pair should be returned    












