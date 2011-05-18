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

data Optimization = TailCall 

type Gensym = String
type Scope = Int
data Identifer = Escaped Gensym Scope | Unescaped Gensym         

type EscapedVars = [Identifier]
type UnescapedVars = [Identifier]
data Decl = FunDec Identifier [Identifier] Expr EscapedVars UnescapedVars [Optimization]
          | VarDec Identifier Expr
  deriving Show

data Expr = Nil
          | Break
          | Num Int
          | Str String
          | Id Identifer
          | Binop Op Expr Expr
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