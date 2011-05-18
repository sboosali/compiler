{-#LANGUAGE PackageImports, RankNTypes #-}
module Escape where

import AST
import qualified Data.Map as M
import qualified OAST as O
import Data.STRef
import Control.Monad.ST
import Data.Map(Map)


unique :: Expr -> O.Expr
unique tree = runST $ do counter <- newSTRef 0
                         let unique' table Nil = return O.Nil
                             unique' table (Num n) = return $ O.Num n
                             unique' table (Identifier id) = case M.lookup id table of
                                                               Nothing  -> fail "omg unqies"
                                                               Just suf -> return $ O.Id (O.Unescaped $ id ++ ';' : show suf)
                             unique' table (Str str) = return $ O.Str str
                             unique' table (Binop op e1 e2) = do oe1 <- unique' table e1
                                                                 oe2 <- unique' table e2
                                                                 return $ O.Binop op oe1 oe2
                             unique' table (Negate e) = fmap O.Negate $ unique' table e
                             unique' table (Seq es) = fmap O.Seq $ mapM (unique' table) es
                             unique' table (While cond body) = do ocond <- unique' table cond
                                                                  obody <- unique' table body
                                                                  return $ O.While ocond obody []
                             unique' table (For id from to body) = do modifySTRef counter (+1)
                                                                      suf <- readSTRef counter
                                                                      let var = O.Unescaped $ id ++ ';' : show suf
                                                                          new_table = M.insert id suf table
                                                                      ofrom <- unique' table from
                                                                      oto <- unique' table to
                                                                      obody <- unique' new_table body -- in the new environment!!
                                                                      return $ O.For var ofrom oto obody []
                             unique' table (Assign lv rv) = do olv <- unique' table lv
                                                               orv <- unique' table rv
                                                               return $ O.Assign olv orv []
                             unique' table (If c t e) = do oc <- unique' table c
                                                           ot <- unique' table t
                                                           oe <- unique' table e
                                                           return $ O.If oc ot oe []
--                           unique' table (RecordCreation id pairs) = ??
--                           unique' table (FieldRef obj field) = 0
                             unique' table (ArrayCreation _ size value) = do osize <- unique' table size
                                                                             ovalue <- unique' table value
                                                                             return $ O.ArrayCreation osize ovalue []
                             unique' table (ArrayRef obj index) =  do oobj <- unique' table obj
                                                                      oindex <- unique' table index
                                                                      return $ O.ArrayRef oobj oindex []
                                                                                           
                             unique' table (Funcall cls args) = do ocls <- unique' table cls
                                                                   oargs <- mapM (unique' table) args
                                                                   return $ O.Funcall ocls oargs []
                             unique' table Break = return O.Break
                             unique' table (Let [] exprs) = do body <- unique' table $ Seq exprs
                                                               return $ O.Let [] body []
                             unique' table (Let ((TypeDec _ _ ):decs) exprs) = unique' table $ Let decs exprs
                             unique' table (Let ((TypedVarDec id _ expr):decs) exprs) = unique' table (Let ((UntypedVarDec id expr):decs) exprs)
                             unique' table (Let ((UntypedVarDec id expr):decs) exprs) = do oexpr <- unique' table expr
                                                                                           modifySTRef counter (+1)
                                                                                           suf <- readSTRef counter
                                                                                           let var = O.Unescaped $ id ++ ';' : show suf
                                                                                               new_table = M.insert id suf table
                                                                                           O.Let new_decs new_expr [] <- unique' new_table (Let decs exprs)
                                                                                           return $ O.Let (O.VarDec var oexpr : new_decs)
                                                                                                          new_expr
                                                                                                          []
                             unique' table (Let ((UntypedFunDec id args body):decs) exprs) = do let n = length args
                                                                                                modifySTRef counter (+1)
                                                                                                suf <- readSTRef counter
                                                                                                modifySTRef counter (+n)
                                                                                                let var = O.Unescaped $ id ++ ';' : show suf
                                                                                                    table' = M.insert id suf table
                                                                                                    (new_vars, table'') = process_args table'
                                                                                                                                       (map fst args)
                                                                                                                                       (suf + 1)
                                                                                                obody <- unique' table'' body -- new environment!
                                                                                                O.Let new_decs new_expr [] <- unique' table''
                                                                                                                                  (Let decs exprs)
                                                                                                return $ O.Let (O.FunDec var
                                                                                                                         new_vars
                                                                                                                         obody
                                                                                                                         [] [] [] : new_decs)
                                                                                                               new_expr
                                                                                                               []
                             process_args :: Map ID Int -> [ID] -> Int -> ([O.Identifier], Map ID Int)
                             process_args table [] init_suf = ([], table)
                             process_args table (id:ids) init_suf = let (new_vars, new_table) = process_args table ids (init_suf + 1)
                                                                    in ((O.Unescaped $ id ++ ';' : show init_suf) : new_vars,
                                                                        M.insert id init_suf new_table)
                                                                        

                            in unique' M.empty tree




