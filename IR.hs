module IR where

import OAST hiding (Binop)
import qualified OAST (Binop)
import Data.Char 
import Prelude hiding (lookup)
import qualified Prelude (lookup)
import Data.Map (Map, insert, lookup, empty)
import Control.Monad (foldM_, state)
import IRSig

-----------------------------
-- Header 
-----------------------------

nil = Mem 1
exit = Label 2
begin = 3

-----------------------------
-----------------------------

-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- TRANSLATE |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=

type StreamsT = [Int]
type BreakpointsT = [Int]
data Context = Context {fundefs :: [FunDef], tacs :: [TAC], gs :: StreamT, bs :: BreakpointsT}

-- c {tacs = tacs c} => c

-------------------

addInstr :: TAC -> State Context
addInstr instr :: do c <- get
                     put $ c {tacs = instr : tacs c}

gensym :: State Context Int
gensym = do c <- get
            let (n:nats) = gs c
            put $ c {gs = nats}
            return n

memId :: String -> State Context String
memId name = do n <- gensym
                return $ Mem (name ++ ";" ++ show n)

mem :: State Context String
mem = do n <- gensym
         return $ Mem (";anon" ++ ";" ++ show n)

label :: String -> State Context
label name = do n <- gensym
                return $ Label (name ++ ";" ++ show n)

nats :: Int -> [Int]
nats n = n : nats (n+1)

initState :: State Context 
initState = State $ Context [] [] (nats begin) exit

translate :: Expr -> ([FunDefs],[TAC])
translate e = let c = execState (translate' e) initState
                  functionDefinitions = fundefs c
                  program = reverse $ tacs c 
              in (functionDefinitions, program)

translate _ = fail "translate expects an OAST.Let as entrypoint" 

------------------------------------------------------------------- 
-- helpers

getGensym :: Identifer -> String
getGensym (Escaped i _) = i
getGensym (Unescaped i) = i

-------------------------------------------------------------------
translateD :: Decl -> State Context Expr
translateD (VarDec name e) = 
    do id <- translate' name
       val <- translate' e
       addInstr $ Store id val

translateD (FunDec name params body escvars unescvars _) = 
    do c <- get
       let instructions = tacs c       
       put $ c {tacs = []} -- clear program instructions
       
       translate' body 
       c <- get
       let funBody = tacs c
       let f = (Mem . getGensym) name
       let ps = map (Mem . getGensym) params

       let functionDefinitions = FunDef f ps funBody escvars unescvars
       put $ c {fundefs = functionDefinition : fundefs c, tacs = instructions} -- add fundef, then restore program instructions

       Mem f <- translate' name -- name :: Id Identifier       
       addInstr $ MakeClosure (Mem f) (Label f)
       return $ Mem f

-------------------------------------------------------------------
translate' :: Expr -> State Context Val
translate' (Num n) = return $ Const n
translate' (Id i) = return $ Mem (getGensym i) -- returns Mem subset of Val

translate' (OAST.Binop op e1 e2) = do dest <- mem
                                      src1 <- translate' e1
                                      src2 <- translate' e2
                                      addInstr $ OAST.Binop (toBinopEnum op) dest src1 src2
                                      return dest

translate' (Seq es) = do locs <- mapM translate' es
                         return $ head $ reverse locs

translate' (Let decls expr _) = do mapM_ translateD decls
                                   return $ translate' expr                                 

translate' (Funcall name args _) = 
    do ret <- mem
       let f = getGensym name
       let as = map (Mem . getGensym) args
       addInstr $ Call (Mem f) as ret 
       return ret

translate' other = fail "unhandled case of OAST in translate':\n" ++ show other ++ "\n seriously, wtf is that?"
-------------------------------------------------------------------

