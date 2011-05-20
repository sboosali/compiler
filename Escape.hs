module Escape where

import AST
import qualified OAST as O

import Data.STRef
import Control.Monad.ST

import qualified Data.Map as M
import Data.Map(Map)

import qualified Data.Set as S
import Data.Set(Set)

import Data.List(isPrefixOf,sortBy)
import Data.Ord(comparing)

builtins = ["getchar",
            "ord",
            "print",
            "size",
            "not",
            "chr",
            "exit",
            "error",
            "abort",
            "flush",
            "itoa",
            "concat",
            "substring"]

data Uniqueness = Numeric Int
                | Builtin

applyUnique :: Uniqueness -> ID -> O.Identifier
applyUnique (Numeric suf) id = O.Undecided $ id ++ '.' : show suf
applyUnique Builtin id = O.Undecided $ "builtin$" ++ id

unique :: Expr -> O.Expr
unique tree = runST $ do counter <- newSTRef 0
                         let unique' table Nil = return O.Nil
                             unique' table (Num n) = return $ O.Num n
                             unique' table (Identifier id) = case M.lookup id table of
                                                               Nothing  -> fail $ "unique renaming: unbound identifier: " ++ show id
                                                               Just suf -> return $ O.Id $ applyUnique suf id
                             unique' table (Str str) = return $ O.Str str
                             unique' table (Binop op e1 e2) = do e1 <- unique' table e1
                                                                 e2 <- unique' table e2
                                                                 return $ O.Binop op e1 e2
                             unique' table (Negate e) = do e <- unique' table e
                                                           return $ O.Binop Subtract (O.Num 0) e
                             unique' table (Seq es) = fmap O.Seq $ mapM (unique' table) es
                             unique' table (While cond body) = do cond <- unique' table cond
                                                                  body <- unique' table body
                                                                  return $ O.While cond body []
                             unique' table (For id from to body) = do modifySTRef counter (+1)
                                                                      suf <- readSTRef counter
                                                                      let var = applyUnique (Numeric suf) id
                                                                          new_table = M.insert id (Numeric suf) table
                                                                      from <- unique' table from
                                                                      to <- unique' table to
                                                                      body <- unique' new_table body -- in the new environment!!
                                                                      return $ O.For var from to body []
                             unique' table (Assign lv rv) = do lv <- unique' table lv
                                                               rv <- unique' table rv
                                                               return $ O.Assign lv rv []
                             unique' table (If c t e) = do c <- unique' table c
                                                           t <- unique' table t
                                                           e <- unique' table e
                                                           return $ O.If c t e []
                             unique' table (RecordCreation _ pairs) = let sorted_pairs = map snd $ sortBy (comparing fst) pairs
                                                                      in fmap O.RecordCreation $ mapM (unique' table) sorted_pairs
                             unique' table (FieldRef obj field) = fail $ "escape: should never get here"
                             unique' table (ArrayCreation _ size value) = do size <- unique' table size
                                                                             value <- unique' table value
                                                                             return $ O.ArrayCreation size value []
                             unique' table (ArrayRef obj index) =  do obj <- unique' table obj
                                                                      index <- unique' table index
                                                                      return $ O.ArrayRef obj index []
                                                                                           
                             unique' table (Funcall cls args) = do cls <- unique' table cls
                                                                   args <- mapM (unique' table) args
                                                                   return $ O.Funcall cls args []
                             unique' table Break = return O.Break
                             unique' table (Let [] exprs) = do body <- unique' table $ Seq exprs
                                                               return $ O.Let [] body []
                             unique' table (Let ((TypeDec _ _ ):decs) exprs) = unique' table $ Let decs exprs
                             unique' table (Let ((TypedVarDec id _ expr):decs) exprs) = unique' table (Let ((UntypedVarDec id expr):decs) exprs)
                             unique' table (Let ((UntypedVarDec id expr):decs) exprs) = do expr <- unique' table expr
                                                                                           modifySTRef counter (+1)
                                                                                           suf <- readSTRef counter
                                                                                           let var = applyUnique (Numeric suf) id
                                                                                               new_table = M.insert id (Numeric suf) table
                                                                                           O.Let new_decs new_expr [] <- unique' new_table (Let decs exprs)
                                                                                           return $ O.Let (O.VarDec var expr : new_decs)
                                                                                                          new_expr
                                                                                                          []
                             unique' table (Let ((TypedFunDec id args _ body):decs) exprs) = unique' table (Let ((UntypedFunDec id args body):decs) exprs)
                             unique' table (Let ((UntypedFunDec id args body):decs) exprs) = do let n = length args
                                                                                                modifySTRef counter (+1)
                                                                                                suf <- readSTRef counter
                                                                                                modifySTRef counter (+n)
                                                                                                let var = applyUnique (Numeric suf) id
                                                                                                    table' = M.insert id (Numeric suf) table
                                                                                                    (new_vars, table'') = process_args table'
                                                                                                                                       (map fst args)
                                                                                                                                       (suf + 1)
                                                                                                body <- unique' table'' body -- new environment!
                                                                                                O.Let new_decs new_expr [] <- unique' table''
                                                                                                                                  (Let decs exprs)
                                                                                                return $ O.Let (O.FunDec var
                                                                                                                         new_vars
                                                                                                                         body
                                                                                                                         [] : new_decs)
                                                                                                               new_expr
                                                                                                               []
                             process_args :: Map ID Uniqueness -> [ID] -> Int -> ([O.Identifier], Map ID Uniqueness)
                             process_args table [] init_suf = ([], table)
                             process_args table (id:ids) init_suf = let (new_vars, new_table) = process_args table ids (init_suf + 1)
                                                                    in (applyUnique (Numeric init_suf) id : new_vars,
                                                                        M.insert id (Numeric init_suf) new_table)
                                                                        

                            in let init_env = foldr (\a env -> M.insert a Builtin env) M.empty builtins
                               in unique' init_env tree

data EscapeStatus = Escaped | Unescaped

type Stack = []
type Scope = Set ID


escape :: O.Expr -> O.Expr
escape tree = runST $ do escapedVars <- newSTRef S.empty
                         let find = find' 0

                             find' count name [] = error $ "escape analysis: couldn't find " ++ name
                             find' count name (top:rest) | name `S.member` top = count
                                                         | otherwise = find' (count + 1) name rest

                             escape' env O.Nil = return O.Nil
                             escape' env O.Break = return O.Break
                             escape' env (O.Num n) = return $ O.Num n
                             escape' env (O.Str s) = return $ O.Str s
                             escape' env (O.Id id) = do let O.Undecided varname = id
                                                            scope = find varname env
                                                        if (scope /= 0) 
                                                           then do modifySTRef escapedVars (S.insert varname)
                                                                   return $ O.Id $ O.Escaped varname scope
                                                           else return $ O.Id id

                             escape' env (O.Binop op e1 e2) = do e1 <- escape' env e1
                                                                 e2 <- escape' env e2
                                                                 return $ O.Binop op e1 e2
                             escape' env (O.Seq exprs) = fmap O.Seq $ mapM (escape' env) exprs
                             escape' env (O.Assign lv rv opt) = do lv <- escape' env lv
                                                                   rv <- escape' env rv
                                                                   return $ O.Assign lv rv opt
                             escape' env (O.RecordCreation exprs) = fmap O.RecordCreation $ mapM (escape' env) exprs
                             escape' env (O.ArrayCreation size value opt) = do size <- escape' env size
                                                                               value <- escape' env value
                                                                               return $ O.ArrayCreation size
                                                                                                        value
                                                                                                        opt
                             escape' env (O.ArrayRef arr index opt) = do arr <- escape' env arr
                                                                         index <- escape' env index
                                                                         return $ O.ArrayRef arr index opt
                             escape' env (O.While cond body opt) = do cond <- escape' env cond
                                                                      body <- escape' env body
                                                                      return $ O.While cond body opt
                             escape' env (O.For id from to body opt) = do from <- escape' env from -- old environment
                                                                          to <- escape' env to
                                                                          let O.Undecided varname = id
                                                                              new_env = extend varname env
                                                                          body <- escape' new_env body
                                                                          return $ O.For id from to body opt
                             escape' env (O.If c t e opt) = do c <- escape' env c
                                                               t <- escape' env t
                                                               e <- escape' env e
                                                               return $ O.If c t e opt
                             escape' env (O.Funcall fun args opt) = do fun <- escape' env fun
                                                                       args <- mapM (escape' env) args
                                                                       return $ O.Funcall fun args opt

                             escape' env (O.Let [] expr opt) = do expr <- escape' env expr
                                                                  return $ O.Let [] expr opt
                             escape' env (O.Let ((O.VarDec id val):decs) expr opt) = do val <- escape' env val
                                                                                        let O.Undecided varname = id
                                                                                            new_env = extend varname env
                                                                                        O.Let decs expr opt <- escape' new_env (O.Let decs expr opt)
                                                                                        return $ O.Let ((O.VarDec id val):decs) expr opt
                             escape' env (O.Let ((O.FunDec id args body opt1):decs) expr opt) = do let body_env = foldr (extend . getName) (S.empty : env) (id:args)
                                                                                                   body <- escape' body_env body
                                                                                                   let new_env = extend (getName id) env
                                                                                                   O.Let decs expr opt <- escape' new_env (O.Let decs expr opt)
                                                                                                   return $ O.Let ((O.FunDec id args body opt1):decs) expr opt
                                                                                      
                                                                                                                                                                
                             extend name [] = error "escape analysis: empty environment"
                             extend name (current : rest) = S.insert name current : rest


                             getName (O.Undecided name) = name 
                            in do new_tree <- escape' [S.fromList $ map ("builtin$"++) builtins] tree
                                  vars <- readSTRef escapedVars
                                  let decideVar :: O.Identifier -> O.Identifier
                                      decideVar (O.Undecided name) | name `S.member` vars = O.Escaped name 0
                                                                   | otherwise = O.Unescaped name
                                      decideVar id = id

                                      decide :: O.Expr -> O.Expr
                                      decide O.Nil = O.Nil
                                      decide O.Break = O.Break
                                      decide (O.Num n) = O.Num n
                                      decide (O.Str s) = O.Str s
                                      decide (O.Id id) = O.Id $ decideVar id

                                      decide (O.Binop op e1 e2) = O.Binop op (decide e1) (decide e2)
                                      decide (O.Seq exprs) = O.Seq $ map decide exprs
                                      decide (O.Assign lv rv opt) = O.Assign (decide lv) (decide rv) opt
                                      decide (O.RecordCreation exprs) = O.RecordCreation $ map decide exprs
                                      decide (O.ArrayCreation size value opt) = O.ArrayCreation (decide size) (decide value) opt
                                      decide (O.ArrayRef arr index opt) = O.ArrayRef (decide arr) (decide index) opt
                                      decide (O.While cond body opt) = O.While (decide cond) (decide body) opt
                                      decide (O.For id from to body opt) = O.For (decideVar id) (decide from) (decide to) (decide body) opt
                                      decide (O.If c t e opt) = O.If (decide c) (decide t) (decide e) opt
                                      decide (O.Funcall fun args opt) = O.Funcall (decide fun) (map decide args) opt

                                      decide (O.Let decs expr opt) = O.Let (map decideDecl decs) (decide expr) opt
                                                                                             
                                      decideDecl :: O.Decl -> O.Decl
                                      decideDecl (O.VarDec id val) = O.VarDec (decideVar id) (decide val)
                                      decideDecl (O.FunDec id args body opt) = O.FunDec (decideVar id) (map decideVar args) (decide body) opt

                                  return $ decide new_tree