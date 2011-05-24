module Semantic where

import AST hiding (ID)
import qualified Data.Map as M
import Data.Map(Map)
import Control.Monad.ST(runST)
import Control.Monad
import Data.STRef
import Data.List(elemIndex,sort)
import qualified Unify as U
import Type
import Op

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
builtins = let values = [("print",     FunctionT [StrT]           VoidT),
                         ("exit",      FunctionT [NumT]           VoidT),
                         ("itoa",      FunctionT [NumT]           StrT),
                         ("getchar",   FunctionT []               StrT),
                         ("ord",       FunctionT [StrT]           NumT),
                         ("chr",       FunctionT [NumT]           StrT),
                         ("size",      FunctionT [StrT]           NumT),
                         ("not",       FunctionT [NumT]           NumT),
                         ("flush",     FunctionT []               VoidT),
                         ("concat",    FunctionT [StrT,StrT]      StrT),
                         ("substring", FunctionT [StrT,NumT,NumT] StrT)]
               types  = [("int",    NumT),
                         ("string", StrT)]
               valueMap = foldr (\(n,t) -> M.insert n t) M.empty values
               typeMap  = foldr (\(n,t) -> M.insert n t) M.empty types
           in SymbolTable valueMap typeMap


semantic :: Expr -> (Type, Expr)
semantic tree = runST $ do cmpCounter <- newSTRef 0
                           let
--                             type_check :: SymbolTable -> Expr -> Type -> TC Type
                               type_check table expr expected = do (actual, new_expr) <- type_of table expr
                                                                   case (actual, expected) of
                                                                     (NilT, RecordT _ _ _) -> return (NilT, new_expr)
                                                                     (r@(RecordT _ _ _), NilT) -> return (r, new_expr)
                                                                     _ -> if actual == expected
                                                                          then return (actual, new_expr)
                                                                          else fail $  "Type mismatch: Expected " ++ show expected
                                                                                   ++ " but got " ++ show actual
                                                                                   ++ "\nIn expression:\n" ++ show expr  


--                             type_of :: SymbolTable -> Expr -> TC Type
                               type_of table Nil = return (NilT, Nil)
                               type_of table (Num n) = return (NumT, Num n)
                               type_of table (Str s) = return (StrT, Str s)
                               type_of table (Identifier id) = case M.lookup id $ valueTable table of
                                                                 Just t  -> return (t, Identifier id)
                                                                 Nothing -> fail $ "Unbound identifier " ++ id
                               type_of table (Binop Equals e1 e2) = do (t1, e1) <- type_of table e1
                                                                       (_, e2) <- type_check table e2 t1
                                                                       return (NumT, Binop Equals e1 e2)
                               type_of table (Binop op e1 e2) = do (_, e1) <- type_check table e1 NumT
                                                                   (_, e2) <- type_check table e2 NumT
                                                                   return (NumT, Binop op e1 e2)

                               type_of table (Negate e) = do (_, e) <- type_check table e NumT
                                                             return (NumT, Negate e)
                                                     
                               type_of table (Seq []) = return (VoidT, Seq [])
                               type_of table (Seq [e1]) = do (t, e1) <- type_of table e1
                                                             return (t, Seq [e1])
                               type_of table (Seq (e:es)) = do (_, e) <- type_of table e
                                                               (t2, Seq es) <- type_of table (Seq es)
                                                               return (t2, Seq (e:es))

                               type_of table (While cond body) = do (_, cond) <- type_check table cond NumT
                                                                    (_, body) <- type_check table body VoidT
                                                                    return (VoidT, While cond body)

                               type_of table (For id from to body) = do (_, from) <- type_check table from NumT
                                                                        (_, to) <- type_check table to NumT
                                                                        (_, body) <- type_check (addValueBinding table id NumT)
                                                                                                body
                                                                                                VoidT
                                                                        return (VoidT, For id from to body)

                               type_of table (Assign lv rv) = do (_, lv) <- type_of table lv
                                                                 (_, rv) <- type_of table rv
                                                                 return (VoidT, Assign lv rv)

                               type_of table (If c t e) = do (_, c) <- type_check table c NumT
                                                             (thenT, t) <- type_of table t
                                                             (_, e) <- type_check table e thenT
                                                             return (thenT, If c t e)

                               type_of table (RecordCreation id bindings) = case M.lookup id $ typeTable table of
                                                                              Just t@(RecordT _ _ fields) -> do bindings <- forM (sort fields) $ \(name, ftype) ->
                                                                                                                               case lookup name bindings of
                                                                                                                                 Just val -> do (_, val) <- type_check table val ftype
                                                                                                                                                return (name, val)
                                                                                                                                 Nothing -> fail $ "Missing field declaration " ++ show name
                                                                                                                return (t, RecordCreation id bindings)
                                                                              Just t -> fail $ "Expected record type at record creation: " ++ show t
                                                                              Nothing -> fail $ "Unbound type identifier " ++ id
  
                               type_of table (ArrayCreation t size init) = do let array_type = lookupTypeID table t
                                                                              case array_type of
                                                                                ArrayT _ entry_type -> do (_, size) <- type_check table size NumT
                                                                                                          (_, init) <- type_check table init entry_type
                                                                                                          return (array_type, ArrayCreation t size init)
                               type_of table (FieldRef obj fieldname) = do (obj_type, obj) <- type_of table obj
                                                                           case obj_type of
                                                                             RecordT _ _ fields -> case lookup fieldname fields of
                                                                                                     Just t -> return (t, ArrayRef obj $Num$getFieldId (map fst fields) fieldname)
                                                                                                     Nothing -> fail $ "Nonexistent field: " ++ show fieldname
                                                                             t -> fail $ "Expected record type at field ref: " ++ show t
                                                                                                                                                              
                               type_of table (ArrayRef obj index) = do (obj_type, obj) <- type_of table obj
                                                                       case obj_type of
                                                                         ArrayT _ t -> do (_, index) <- type_check table index NumT
                                                                                          return (t, ArrayRef obj index)
                                                                         t -> fail $ "Expected array type at array ref: " ++ show t
                               type_of table (Funcall fun args) = do (fun_type, fun) <- type_of table fun
                                                                     case fun_type of
                                                                       FunctionT arg_types ret -> do args <- forM (zip args arg_types) $ \(arg, arg_type) ->
                                                                                                                   do (_, arg) <- type_check table arg arg_type
                                                                                                                      return arg
                                                                                                     return (ret, Funcall fun args)
                                                                       t -> fail $ "Expected function type at function call: " ++ show t
                               type_of table Break = return (VoidT, Break)

                               {- LET -}
                               -- U.check :: Expr -> (Expr, Map ID Type)
                               type_of table (Let [] es) = do (t, Seq es) <- type_of table (Seq es)
                                                              return (t, Let [] es)

                               type_of table oldLet@(Let (d@(TypeDec tname synType):_) es) = do let (newLet, types) = U.check oldLet
                                                                                                    -- integrate into current env
                                                                                                    bindings = M.assocs (typeTable table)
                                                                                                    sub (tname, tval) t = fmap (U.swap tname tval) t 
                                                                                                    iterate' [] t = t
                                                                                                    iterate' (binding:bs) t = iterate' bs (sub binding t)
                                                                                                    repeat 0 bs t = t
                                                                                                    repeat n bs t = let r = repeat (n-1) bs t 
                                                                                                                        bs' = M.assocs r
                                                                                                                    in iterate' bs' r
                                                                                                    integratedTypes = repeat 100 bindings types
                                                                                                    recurrences = iterate' bindings $ M.fromList $
                                                                                                                    map (\(n,t) -> (n, U.recur n t)) $ M.toList integratedTypes
                                                                                                    newEnv = table {typeTable = M.union recurrences (typeTable table)}
                                                                                                    -- check for type ids yet unbound

                                                                                                if U.anyUnbound recurrences
                                                                                                  then fail $ "Escaped unbound: " ++ show recurrences
                                                                                                  else type_of newEnv newLet
                                                                                                    
                               type_of table (Let ((UntypedVarDec id expr):decs) es) = do (expr_type, expr) <- type_of table expr
                                                                                          (t, Let decs es) <- type_of (addValueBinding table id expr_type) (Let decs es)
                                                                                          return (t, Let ((UntypedVarDec id expr):decs) es)
                               type_of table (Let ((TypedVarDec id expected_type expr):decs) es) = do let var_type = lookupTypeID table expected_type
                                                                                                      (_, expr) <- type_check table expr var_type
                                                                                                      (t, Let decs es) <- type_of (addValueBinding table id var_type)
                                                                                                                                  (Let decs es)
                                                                                                      return (t, Let ((TypedVarDec id expected_type expr):decs) es)
                               type_of table (Let ((UntypedFunDec fun args body):decs) es) = do let arg_names = map fst args
                                                                                                    arg_types = map (lookupTypeID table . snd) args
                                                                                                    body_env = foldr (\(n,t) env -> addValueBinding env n t)
                                                                                                                     table 
                                                                                                                     (zip arg_names arg_types)
                                                                                                (return_type, body) <- type_of body_env body
                                                                                                let funType = FunctionT arg_types return_type
                                                                                                    new_table = addValueBinding table fun funType
                                                                                                (t, Let decs es) <- type_of new_table (Let decs es)
                                                                                                return (t, Let ((UntypedFunDec fun args body):decs) es)
                               type_of table (Let ((TypedFunDec fun args returns body):decs) es) = do let arg_names = map fst args
                                                                                                          arg_types = map (lookupTypeID table . snd) args
                                                                                                          return_type = lookupTypeID table returns
                                                                                                          funType = FunctionT arg_types return_type
                                                                                                          body_env = foldr (\(n,t) env -> addValueBinding env n t)
                                                                                                                           table 
                                                                                                                          (zip arg_names arg_types)
                                                                                                          body_env' = addValueBinding body_env fun funType
                                                                                                          new_table = addValueBinding table fun funType
                                                                                                      (_, body) <- type_check body_env' body return_type
                                                                                                      (t, Let decs es) <- type_of new_table (Let decs es)
                                                                                                      return (t, Let ((TypedFunDec fun args returns body):decs) es)
                               {- -}

                               getFieldId fields fieldname = case fieldname `elemIndex` sort fields of
                                                               Nothing -> error $ "unknown field member " ++ show fieldname
                                                               Just i -> i

                               parseSyntacticType table (VarST id) = return $ lookupTypeID table id
                               parseSyntacticType table (ArrayST id) = do modifySTRef cmpCounter (+1)
                                                                          cmp <- readSTRef cmpCounter
                                                                          return $ ArrayT cmp (lookupTypeID table id)
                               parseSyntacticType table (RecordST fields) = do modifySTRef cmpCounter (+1)
                                                                               cmp <- readSTRef cmpCounter
                                                                               let names = map fst fields
                                                                                   types = map (lookupTypeID table . snd) fields
                                                                               return $ RecordT cmp False $ zip names types
                               parseSyntacticType table (FunST args return_type) = do args <- mapM (parseSyntacticType table) args
                                                                                      return_type <- parseSyntacticType table return_type
                                                                                      return $ FunctionT args return_type
                                                                                       
                               lookupTypeID table id = case M.lookup id (typeTable table) of
                                                         Just t -> t
                                                         Nothing -> error $ "lookupTypeID: Unbound type identifier: " ++ show id
                            in type_of builtins tree