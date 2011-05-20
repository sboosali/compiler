module IRSig where
import Op
import Prelude hiding (LT, EQ, GT)
import OAST 

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

data Val = Const Int 
         | Mem Gensym 
         | MemOffset Val Val
         deriving Show

-- MemOffset encapsulate pointer arithmetic. The first Gensym is a location in memory. The second Gensym stores an offset from that location in memory.
-- i.e. MemOffset a::Value b::Address
-- Mem a => Deref a
-- MemOffset a b => Deref (Deref a + (Deref b)) i.e. Interpret a as Address, for the intermediary computation
-- MemOffset Gensym Gensym (as opposed to MemOffset Gensym Int) allows IR to avoid doing pointer arithmetic, and to not deal with dereferncing (i.e. pointer / value dichotomy).

type Offset = Val
type Scope = Int
type Size = Val   
type Dest = Val
type Src = Val
type Env = Val 
type Return = Val
type Pointer = Val
type Fun = Val
type Arg = Val
data Label = Label String deriving Show

data FunDef = FunDef Label [Val] [TAC]
            deriving Show
data TAC = LabelDecl Label
 | Malloc Val Val -- Malloc size location
 | AllocateString String Val

 | BinopInstr BinopEnum Dest Src Src 
 | Relop RelopEnum Dest Src Src

 | Jump Label
 | Cjump RelopEnum Src Src Label Label

 -- Semantics of Move: Dest and Src are in memory
 -- (but with a Src==Const, it's a load immediate)
 -- that is, they are pointers
 -- move the value pointed to by src into the location pointed to by dest
 | Move Dest Src

 | Call Fun [Arg] Return
 | MakeClosure Pointer Label 
           deriving Show