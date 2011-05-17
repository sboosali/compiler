{-#LANGUAGE PackageImports #-}
module Semantic where

import AST
import qualified Data.Map as M
import Data.Map(Map)
import "mtl" Control.Monad.Error



data Type = ArrayT Type
          | RecordT [(ID, Type)]
          | FunctionT [Type] Type
          | NilT
          | NumT
          | StrT
          | VoidT
  deriving (Eq, Show, Ord)

data SymbolTable = SymbolTable {valueTable :: Map ID Type,
                                typeTable  :: Map ID Type}
    deriving (Eq, Show, Ord)

empty :: SymbolTable
empty = SymbolTable M.empty M.empty

addValueBinding :: SymbolTable -> ID -> Type -> SymbolTable
addValueBinding (SymbolTable vt tt) id t = SymbolTable (M.insert id t vt) tt

addTypeBinding :: SymbolTable -> ID -> Type -> SymbolTable
addTypeBinding (SymbolTable vt tt) id t = SymbolTable vt (M.insert id t tt)


type TC = Either String

{- make sure to fix nil behavior -}
check :: Type -> Type -> TC ()
check actual expected = if actual == expected
                        then return ()
                        else throwError $ "type mismatch: expected "
                                 ++ show expected ++ " but got " ++ show actual
 
type_check :: SymbolTable -> Expr -> Type -> TC Type
type_check table expr expected = do
  actual <- type_of table expr
  if actual == expected
     then return actual
     else throwError $  "Type mismatch: Expected " ++ show expected
                     ++ " but got " ++ show actual
                     ++ "\nIn expression:\n" ++ show expr  


type_of :: SymbolTable -> Expr -> TC Type
type_of table Nil = return NilT
type_of table (Num _) = return NumT
type_of table (Str _) = return StrT
type_of table (Identifier id) = case M.lookup id $ valueTable table of
                                  Just t  -> return t
                                  Nothing -> throwError $ "Unbound identifier " ++ id
type_of table (Binop _ e1 e2) = do type_check table e1 NumT
                                   type_check table e2 NumT
                                   return NumT

type_of table (Negate e) = type_check table e NumT

type_of table (Seq []) = return VoidT
type_of table (Seq [e1]) = type_of table e1
type_of table (Seq (e:es)) = type_of table e >> type_of table (Seq es)

type_of table (While cond body) = do type_check table cond NumT
                                     type_check table body VoidT
                                     return VoidT

type_of table (For id from to body) = do type_check table from NumT
                                         type_check table to NumT
                                         type_check (addValueBinding table id NumT)
                                                    body
                                                    VoidT
                                         return VoidT

type_of table (Assign lv rv) = return VoidT {- Complicated -}

type_of table (If c t e) = do type_check table c NumT
                              thenT <- type_of table t
                              type_check table e thenT
                              return thenT

type_of table (RecordCreation id bindings) = do
  
type_of table (ArrayCreation ...) =
type_of table (FieldRef ...) =
type_of table (ArrayRef ...) =
type_of table (Funcall ...) = 
type_of table Break = return VoidT
type_of table (Let _ es) = type_of table (Seq es)
