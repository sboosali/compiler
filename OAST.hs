module OAST where

{-
throw away types 
records & fields -> arrays & offets
-}

data Op = Add
        | Multiply
        | Subtract
        | Divide
        | LessThan
        | LessEq
        | Equals
        | NotEq
        | GreaterThan
        | GreaterEq
        | And
        | Or
  deriving Show

type Gensym = Int
type Scope = Int


            -- (for the one codegen we've implemented, i.e. MIPS)
            -- offset from stack frame, stack frames up (0=current,1=parents,etc.)
            -- unique ids:
            -- a new fundef 'resets' the stack frames up
            -- i.e. new vars are of the form [0,1...] 
            -- while old vals take on the form [sfu+1,i]
            -- so only keep track of 'current offset' (which is reset to 0 with each new fundef)
            -- and map ids to [offset,sf] pairs (where offset is offset+1 for each new fundef)

data Optimization = TailCall -- TCO -- any function that ends EITHER in a funcall OR an expression pristine of any funcalls (using type info)
                  | EscapingFor -- for that mutates incrementee

-- (String is original var name, for human-readable assembly / debugging)
data Identifer = Outer String OuterOffset Gensym Scope  -- 'scope' stack frames up, using the static links  
               | Inner String Gensym         

data Decl = FunDec Identifier [Identifier] Expr [Identifier] [Optimization]
          | VarDec Identifier Expr
  deriving Show
data Expr = Nil
          | Break
          | Num Int
          | Str String
          | Id Identifer          | Binop Op Expr Expr
          | Negate Expr 
          | Seq [Expr]
          | Assign Expr Expr [Optimization]  
          | Let [Decl] [Expr] [Optimization]  
          | RecordCreation Expr Int [Expr]
          | ArrayCreation Expr Expr [Optimization]  
          | ArrayRef Expr Expr [Optimization] 
          | While Expr Expr [Optimization] 
          | For Identifer Expr Expr Expr  [Optimization]
          | If Expr Expr Expr [Optimization]   
          | Funcall Expr [Expr] [Optimization]  
  deriving Show