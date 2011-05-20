module OAST where
import Op

{-
throw away types 
records & fields -> arrays & offets
-}

data Optimization = TailCall 
                  deriving Show

type Gensym = String
type Scope = Int
data Identifier = Escaped Gensym Scope | Unescaped Gensym | Undecided Gensym
            deriving Show

data Decl = FunDec Identifier [Identifier] Expr [Optimization]
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
          | Let [Decl] Expr [Optimization]  
          | RecordCreation [Expr]
          | ArrayCreation Expr Expr [Optimization]  
          | ArrayRef Expr Expr [Optimization] 
          | While Expr Expr [Optimization] 
          | For Identifier Expr Expr Expr  [Optimization]
          | If Expr Expr Expr [Optimization]   
          | Funcall Expr [Expr] [Optimization]  
  deriving Show

