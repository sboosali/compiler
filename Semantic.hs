module Semantic where

import AST
import qualified Data.Map as M
import Data.Map(Map)
import Control.Monad.ST(runST)
import Control.Monad
import Data.STRef

type Cmp = Int -- Unique ID for type comparison

data Type = ArrayT Cmp Type
          | RecordT Cmp [(ID, Type)]
          | FunctionT [Type] Type
          | NilT
          | NumT
          | StrT
          | VoidT
  deriving (Show, Ord)

instance Eq Type where
   (ArrayT n1 _) == (ArrayT n2 _) = n1 == n2
   (RecordT n1 _) == (RecordT n2 _) = n1 == n2
   (FunctionT args1 ret1) == (FunctionT args2 ret2) = args1 == args2 && ret1 == ret2
   NilT == NilT = True
   NumT == NumT = True
   StrT == StrT = True
   VoidT == VoidT = True
   _ == _ = False

data SymbolTable = SymbolTable {valueTable :: Map ID Type,
                                typeTable  :: Map ID Type}
    deriving (Eq, Show, Ord)

addValueBinding :: SymbolTable -> ID -> Type -> SymbolTable
addValueBinding (SymbolTable vt tt) id t = SymbolTable (M.insert id t vt) tt

addTypeBinding :: SymbolTable -> ID -> Type -> SymbolTable
addTypeBinding (SymbolTable vt tt) id t = SymbolTable vt (M.insert id t tt)

empty :: SymbolTable
empty = SymbolTable M.empty M.empty

builtins :: SymbolTable
builtins = let values = [("print",   FunctionT [StrT] VoidT),
                         ("exit",    FunctionT [NumT] VoidT),
                         ("itoa",    FunctionT [NumT] StrT),
                         ("getchar", FunctionT []     StrT)]
               types  = [("int",    NumT),
                         ("string", StrT)]
               valueMap = foldr (\(n,t) -> M.insert n t) M.empty values
               typeMap  = foldr (\(n,t) -> M.insert n t) M.empty types
           in SymbolTable valueMap typeMap


semantic :: Expr -> Type
semantic tree = runST $ do cmpCounter <- newSTRef 0
                           let check NilT (RecordT _ _) = return ()
                               check actual expected = if actual == expected
                                                       then return ()
                                                       else fail $ "type mismatch: expected "
                                                                ++ show expected ++ " but got " ++ show actual
 
--                             type_check :: SymbolTable -> Expr -> Type -> TC Type
                               type_check table expr expected = do actual <- type_of table expr
                                                                   case (actual, expected) of
                                                                     (NilT, (RecordT _ _)) -> return NilT
                                                                     _ -> if actual == expected
                                                                          then return actual
                                                                          else fail $  "Type mismatch: Expected " ++ show expected
                                                                                   ++ " but got " ++ show actual
                                                                                   ++ "\nIn expression:\n" ++ show expr  


--                             type_of :: SymbolTable -> Expr -> TC Type
                               type_of table Nil = return NilT
                               type_of table (Num _) = return NumT
                               type_of table (Str _) = return StrT
                               type_of table (Identifier id) = case M.lookup id $ valueTable table of
                                                                 Just t  -> return t
                                                                 Nothing -> fail $ "Unbound identifier " ++ id
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

                               type_of table (RecordCreation id bindings) = case M.lookup id $ typeTable table of
                                                                              Just t@(RecordT _ fields) -> do forM fields $ \(name, ftype) ->
                                                                                                                   case lookup name bindings of
                                                                                                                     Just val -> type_check table val ftype
                                                                                                                     Nothing -> fail $ "Missing field declaration " ++ show name
                                                                                                              return t
                                                                              Just t -> fail $ "Expected record type at record creation: " ++ show t
                                                                              Nothing -> fail $ "Unbound type identifier " ++ id
  
                               type_of table (ArrayCreation t size init) = do let entry_type = lookupTypeID table t
                                                                              type_check table size NumT
                                                                              type_check table init entry_type
                                                                              modifySTRef cmpCounter (+1)
                                                                              cmp <- readSTRef cmpCounter
                                                                              return $ ArrayT cmp entry_type
                               type_of table (FieldRef obj fieldname) = do obj_type <- type_of table obj
                                                                           case obj_type of
                                                                             RecordT _ fields -> case lookup fieldname fields of
                                                                                                   Just t -> return t
                                                                                                   Nothing -> fail $ "Nonexistent field: " ++ show fieldname
                                                                             t -> fail $ "Expected record type at field ref: " ++ show t
                                                                                                                                                              
                               type_of table (ArrayRef obj index) = do obj_type <- type_of table obj
                                                                       case obj_type of
                                                                         ArrayT _ t -> do type_check table index NumT
                                                                                          return t
                                                                         t -> fail $ "Expected array type at array ref: " ++ show t
                               type_of table (Funcall fun args) = do fun_type <- type_of table fun
                                                                     case fun_type of
                                                                       FunctionT arg_types ret -> do forM (zip args arg_types) $ \(arg, arg_type) ->
                                                                                                         type_check table arg arg_type
                                                                                                     return ret
                                                                       t -> fail $ "Expected function type at function call: " ++ show t
                               type_of table Break = return VoidT
                               type_of table (Let [] es) = type_of table (Seq es)
                               type_of table (Let ((TypeDec tname synType):decs) es) = do var_type <- parseSyntacticType table synType
                                                                                          let new_table = addTypeBinding table tname var_type
                                                                                          type_of new_table (Let decs es)
                               type_of table (Let ((UntypedVarDec id expr):decs) es) = do expr_type <- type_of table expr
                                                                                          type_of (addValueBinding table id expr_type) (Let decs es)
                               type_of table (Let ((TypedVarDec id expected_type expr):decs) es) = do let var_type = lookupTypeID table expected_type
                                                                                                      type_check table expr var_type
                                                                                                      type_of (addValueBinding table id var_type) (Let decs es)
                               type_of table (Let ((UntypedFunDec fun args body):decs) es) = do let arg_names = map fst args
                                                                                                    arg_types = map (lookupTypeID table . snd) args
                                                                                                    body_env = foldr (\(n,t) env -> addValueBinding env n t)
                                                                                                                     table 
                                                                                                                     (zip arg_names arg_types)
                                                                                                return_type <- type_of body_env body
                                                                                                let funType = FunctionT arg_types return_type
                                                                                                    new_table = addValueBinding table fun funType
                                                                                                type_of new_table (Let decs es)
                               type_of table (Let ((TypedFunDec fun args returns body):decs) es) = do let arg_names = map fst args
                                                                                                          arg_types = map (lookupTypeID table . snd) args
                                                                                                          return_type = lookupTypeID table returns
                                                                                                          funType = FunctionT arg_types return_type
                                                                                                          body_env = foldr (\(n,t) env -> addValueBinding env n t)
                                                                                                                           table 
                                                                                                                           (zip arg_names arg_types)
                                                                                                          body_env' = addValueBinding body_env fun funType
                                                                                                          new_table = addValueBinding table fun funType
                                                                                                      type_check body_env' body return_type
                                                                                                      type_of new_table (Let decs es)

                               parseSyntacticType table (VarST id) = return $ lookupTypeID table id
                               parseSyntacticType table (ArrayST id) = do modifySTRef cmpCounter (+1)
                                                                          cmp <- readSTRef cmpCounter
                                                                          return $ ArrayT cmp (lookupTypeID table id)
                               parseSyntacticType table (RecordST fields) = do modifySTRef cmpCounter (+1)
                                                                               cmp <- readSTRef cmpCounter
                                                                               let names = map fst fields
                                                                                   types = map (lookupTypeID table . snd) fields
                                                                               return $ RecordT cmp $ zip names types
                               parseSyntacticType table (FunST args return_type) = do args <- mapM (parseSyntacticType table) args
                                                                                      return_type <- parseSyntacticType table return_type
                                                                                      return $ FunctionT args return_type
                                                                                       
                               lookupTypeID table id = case M.lookup id (typeTable table) of
                                                         Just t -> t
                                                         Nothing -> error $ "lookupTypeID: Unbound type identifier: " ++ show id
                            in type_of builtins tree