module OAST where
import AST(Op)

{-
throw away types 
records & fields -> arrays & offets
-}



data Optimization = TailCall deriving (Eq, Show)

type Gensym = String
type Scope = Int
data Identifier = Escaped Gensym Scope | Unescaped Gensym
                deriving (Eq, Show)

type EscapedVars = [Identifier]
type UnescapedVars = [Identifier]
data Decl = FunDec Identifier [Identifier] Expr EscapedVars UnescapedVars [Optimization]
          | VarDec Identifier Expr
  deriving Show

data Expr = Nil
          | Break
          | Num Int
          | Str String
          | Id Identifier
          | Binop Op Expr Expr
          | Negate Expr 
          | Seq [Expr]
          | Assign Expr Expr [Optimization]  
          | Let [Decl] [Expr] [Optimization]  
          | RecordCreation Expr Int [Expr]
          | ArrayCreation Expr Expr [Optimization]  
          | ArrayRef Expr Expr [Optimization] 
          | While Expr Expr [Optimization] 
          | For Identifier Expr Expr Expr  [Optimization]
          | If Expr Expr Expr [Optimization]   
          | Funcall Expr [Expr] [Optimization]  
  deriving Show