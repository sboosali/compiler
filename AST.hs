module AST(module AST, module Op) where
import Op

data Token = TokenInteger Int
           | TokenString String
           | TokenID String
           | TokenTypeID String
           | TokenType
           | TokenArrow
           | TokenArray
           | TokenOf
           | TokenVar
           | TokenFunction
           | TokenNil
           | TokenBreak
           | TokenLet
           | TokenIn
           | TokenEnd
           | TokenEq
           | TokenComma
           | TokenColon
           | TokenSemicolon
           | TokenAssign
           | TokenDot
           | TokenLBrace
           | TokenRBrace
           | TokenLParen
           | TokenRParen
           | TokenLBracket
           | TokenRBracket
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenFor
           | TokenWhile
           | TokenDo
           | TokenTo
           | TokenTimes
           | TokenDivide
           | TokenPlus
           | TokenMinus
           | TokenNeq
           | TokenLT
           | TokenGT
           | TokenLEQ
           | TokenGEQ
           | TokenAnd
           | TokenOr
  deriving (Show, Ord, Eq)

-- AST
type ID = String
type TypeID = String
data SyntacticType = VarST TypeID
                   | ArrayST TypeID
                   | RecordST [(ID, TypeID)]
                   | FunST [SyntacticType] SyntacticType
  deriving Show

data Decl = TypeDec ID SyntacticType
          | UntypedFunDec ID [(ID, TypeID)] Expr
          | TypedFunDec ID [(ID, TypeID)] TypeID Expr
          | UntypedVarDec ID Expr
          | TypedVarDec ID TypeID Expr
  deriving Show

data Expr = Nil
          | Num Int
          | Identifier ID
          | Str String
          | Binop Op Expr Expr
          | Negate Expr
          | Seq [Expr]
          | While Expr Expr
          | For ID Expr Expr Expr
          | Assign Expr Expr
          | If Expr Expr Expr
          | RecordCreation ID [(ID, Expr)]
          | ArrayCreation TypeID Expr Expr
          | FieldRef Expr ID
          | ArrayRef Expr Expr
          | Funcall Expr [Expr]
          | Break
          | Let [Decl] [Expr]
  deriving Show