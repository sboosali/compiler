module Unify where
import qualified Data.Map as M
import AST hiding (ID)
import Data.List (sort,foldl')
import Prelude hiding (iterate)
import Type

type Table = M.Map ID Type
unify :: Table -> Table
unify = unify' 100
   
unify' 0 types = types 
unify' n tableP = unify' (n-1) (iterate tableP)

substitute :: Table -> (ID,Type) -> Table  -- don't recur on self; %TODO explicitly handle self-recursive types                    
substitute table (tname,tval) = M.insert tname tval (fmap (swap tname tval) $ M.delete tname table)
             
swap :: ID -> Type -> Type -> Type
swap this forThat here = swap' here
    where swap' u@(UnboundT id) =           if id == this then forThat else u
          swap' (ArrayT cmp t) =            ArrayT cmp $ swap' t
          swap' (RecordT cmp inf fields) =  RecordT cmp inf $ zip (map fst fields) (map swap' $ map snd fields)
          swap' (FunctionT args body) =     FunctionT (map swap' args) (swap' body)
          swap' flat =                      flat   

iterate table = iterate' table (M.toList table)
iterate' table [] = table
iterate' table (binding:bs) = iterate' (substitute table binding) bs

anyUnbound table = or (map containsUnbound $ map snd (M.toList table))
 
containsUnbound (ArrayT cmp t) =        containsUnbound t
containsUnbound (RecordT cmp False fields) =  or (map containsUnbound (map snd fields))
containsUnbound (RecordT cmp True _) = False
containsUnbound (FunctionT args body) = or (containsUnbound body : map containsUnbound args)
containsUnbound (UnboundT _) =          True
containsUnbound _ =                     False                    

---------------------------------
cmp = 0

-- mututally recursive (MR)
parseSyntacticTypeMR (VarST id) = lookupTypeIDMR id

parseSyntacticTypeMR (ArrayST id) = ArrayT cmp (lookupTypeIDMR id)

parseSyntacticTypeMR (RecordST fields) = let names = map fst fields
                                             types = map (lookupTypeIDMR . snd) fields
                                         in RecordT cmp False $ zip names types

parseSyntacticTypeMR (FunST args return_type) = let args' = map parseSyntacticTypeMR args
                                                    return_type' = parseSyntacticTypeMR return_type
                                                in FunctionT args' return_type'
                                                          
lookupTypeIDMR = UnboundT
                                                  
pullTypeDecs :: [Decl] -> ([Decl],[Decl]) -- (contiguous type decs , remainder)
pullTypeDecs decs = pullTypeDecsMR decs []
    where pullTypeDecsMR [] typeDecs = ([], reverse typeDecs)
          pullTypeDecsMR (td@(TypeDec _ _):decs) typeDecs = pullTypeDecsMR decs (td:typeDecs)
          pullTypeDecsMR decs typeDecs = (decs, reverse typeDecs)
---------------------------------                            

checkD [] types = unify types
checkD ((TypeDec tname synType):decls) types = let var_type = parseSyntacticTypeMR synType -- type string = int
                                                   types' = M.insert tname var_type types
                                               in checkD decls types'
checkD decs _ = error $ show decs ++ "expects only TypeDecs"
                                                                           
check :: Expr -> (Expr,Table)
check (Let decls e) = let (ds,type_ds) = (pullTypeDecs decls)
                      in (Let ds e, checkD type_ds M.empty)

check _ = error "expects Let as entrypoint"


selfSub :: Type -> ID -> Type -> Type
selfSub self id (ArrayT n t) = ArrayT n (selfSub self id t)
selfSub self id (RecordT n _ bindings) = let ids = map fst bindings
                                             types = map snd bindings
                                         in RecordT n True $ zip ids $ map (selfSub self id) types
selfSub self id (FunctionT args ret) = let args' = map (selfSub self id) args
                                           ret' = selfSub self id ret
                                       in FunctionT args' ret'
selfSub _ _ NilT = NilT
selfSub _ _ NumT = NumT
selfSub _ _ StrT = StrT
selfSub _ _ VoidT = VoidT
selfSub self id (UnboundT s) | s == id = self
                             | otherwise = UnboundT s


fix y = y (fix y)

recur id tval = fix $ \t -> selfSub t id tval