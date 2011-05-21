module IR where

import OAST
import Data.Char 
import Prelude hiding (lookup, LT, EQ, GT, error)
import Data.Map (Map, insert, lookup, empty)
import Control.Monad (mapM, mapM_, foldM_)
import Control.Monad.State
import IRSig
import Op

-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- TRANSLATE' |=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- =|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=
-- Run Time Checks
-- 1) null pointer @ assignment (can be assigned to null, but cannot write to (offset from) null
-- 2) array out of bounds exception @ arrayRef (string? substring is in tiger SL)
-- should array[-1] return length? (with i<=-2 throwing an arrayOutOfBounds exception)

------------------------------------------------------------------- 
-- helpers

-- Stdlib

prefix = "a_"

printString = Label $ prefix++"print"
flush = Label $ prefix++"flush"
getChar = Label $ prefix++"getChar"
chr = Label $ prefix++"chr"
size = Label $ prefix++"size"
substring = Label $ prefix++"substring"
concat = Label $ prefix++"concat"
not = Label $ prefix++"not"
exit = Label $ prefix++"exit"
error = Label $ "error"

--

nil = Const 0
void = Mem "" -- shouldn't be used, just for type checking

inc :: Val -> State Context Val -- usage = a <- inc a
inc (Const n) = do return $ Const (n+1)
inc m = do addInstr $ BinopInstr PLUS m m (Const 1)
           return m

dec (Const n) = do return $ Const (n-1)
dec m = do addInstr $ BinopInstr MINUS m m (Const 1)
           return m


toMem :: Identifier -> Val
toMem (Escaped gs scope) = MemScope gs scope
toMem (Unescaped gs) = Mem gs 

toLabel (Escaped gs scope) = Label gs
toLabel (Unescaped gs) = Label gs

type StreamT = [Int]
type BreakpointsT = [Label]
data Context = Context {fundefs :: [FunDef], tacs :: [TAC], gs :: StreamT, bs :: BreakpointsT}
-- c {tacs = tacs c} => c

pushBreakpoint :: Label -> State Context ()
pushBreakpoint l = do c <- get
                      put $ c {bs = l : bs c}

popBreakpoint :: State Context ()
popBreakpoint = do c <- get
                   put $ c {bs = tail (bs c)}

addInstr :: TAC -> State Context () -- cons'd onto, so list of intrsuctions is backwards
addInstr instr = do c <- get
                    put $ c {tacs = instr : tacs c}

addLabel :: Label -> State Context ()
addLabel l = do addInstr $ LabelDecl l

gensym :: State Context Int
gensym = do c <- get
            let (n:nats) = gs c
            put $ c {gs = nats}
            return n

memName :: String -> State Context Val
memName name = do n <- gensym
                  return $ Mem (name ++ "_internal$" ++ show n)

mem :: State Context Val
mem = do n <- gensym
         return $ Mem ("internal$" ++ show n)

label :: String -> State Context Label
label name = do n <- gensym
                return $ Label (name ++ ".." ++ show n)

nats :: Int -> [Int]
nats n = n : nats (n+1)

initState :: Context 
initState = Context [] [] (nats 0) [exit] 

getGensym :: Identifier -> String
getGensym (Escaped gs scope) = gs
getGensym (Unescaped gs) = gs 

--comment chunking expr = do addInstr $ --Comment chunking expr

-------------------------------------------------------------------
-- translate
translate :: Expr -> ([FunDef],[TAC])
translate e = let c = execState (translate' e) initState
                  functionDefinitions = fundefs c
                  program = reverse $ tacs c 
              in (functionDefinitions, program)

getFundefs = fst . translate
getProgram = snd . translate

-------------------------------------------------------------------
-- translateD
translateD :: Decl -> State Context Val
translateD (VarDec name e) = 
    do let var = toMem name
       val <- translate' e
       addInstr $ Move var val
       return void

translateD (FunDec name params body _) = 
    do c <- get
       let instructions = tacs c       
       put $ c {tacs = []} -- clear program instructions
       
       ret <- translate' body 
       c <- get
       let funBody = reverse $ Return ret : tacs c
       let fmem = toMem name
       let flabel = toLabel name
       
       let ps = map toMem params

       let functionDefinition = FunDef flabel ps funBody
       put $ c {fundefs = functionDefinition : fundefs c, tacs = instructions} -- add fundef, then restore program instructions

       addInstr $ MakeClosure fmem flabel  
       return void

-------------------------------------------------------------------
-- translate' 

translate' :: Expr -> State Context Val
translate' (Num n) = do return $ Const n
translate' (Id i) = do return $ toMem i
translate' (Nil) = do return nil
translate' (Break) = do c <- get
                        addInstr $ Jump $ head $ bs c
                        return nil
translate' (Binop op e1 e2) = do --comment Enter "Binop"

                                 dest <- mem
                                 src1 <- translate' e1
                                 src2 <- translate' e2
                                 addInstr $ BinopInstr (toBinopEnum op) dest src1 src2

                                 --comment Exit "Binop"
                                 return dest

translate' (Str s) = do --comment Enter "Str"
                        addr <- mem
                        addInstr $ AllocateString s addr
                        --comment Exit "Str"
                        return addr

translate' (Seq es) = do --comment Enter "Seq"
                         es <- mapM translate' es
                         --comment Exit "Seq"
                         return $ head $ reverse $ es 

translate' (Let decls expr _) = do --comment Enter "Let"
                                   mapM_ translateD decls
                                   body <- translate' expr 
                                   --comment Exit "Let" 
                                   return body

translate' (Assign lval rval _) = 
    do --comment Enter "Assign"

       rhs <- translate' rval
       lhs <- computeLval lval

       addInstr $ Move lhs rhs

       --comment Exit "Assign"
       return void

    where computeLval :: Expr -> State Context Val
          computeLval (Id i) = do let left = toMem i
                                  stay <- label "stay"
                                  -- runtime check: assignment to nil pointer
                                  addInstr $ Error Nil_pointer_assignment                                  
                                  addInstr $ Cjump EQ left nil error stay
                                  addLabel stay
                                  return left

          computeLval (ArrayRef array offset _) = do offset <- translate' offset
                                                     array <- computeLval array
                                                     return $ MemOffset array offset

translate' (ArrayRef array offset _) = 
    do --comment Enter "ArrayRef"

       array <- translate' array -- array[0] => size
       let size = MemOffset array (Const 0)
       offset <- translate' offset
       stay1 <- label "stay1"
       stay2 <- label "stay2"
       stay3 <- label "stay3"

       -- runtime check: nil pointer derefence
       addInstr $ Error Nil_pointer_dereference
       addInstr $ Cjump EQ array nil error stay1

       -- runtime check: array out of bounds       
       addLabel stay1
       addInstr $ Error Array_out_of_bounds
       addInstr $ Cjump GE offset size error stay2

       addLabel stay2
       addInstr $ Error Array_out_of_bounds
       addInstr $ Cjump LT offset (Const 0) error stay3

       addLabel stay3
       offset <- inc offset -- tiger_array[i] is actually assembly_array[i+1]
 
       --comment Exit "ArrayRef"       
       return $ MemOffset array offset

translate' (RecordCreation fields) = 
    do --comment Enter "RecordCreation"

       record <- mem
       locs <- mapM translate' fields
       let size = length locs 

       addInstr $ Malloc (Const size) record
       let mkField (field, i) = do addInstr $ Move (MemOffset record (Const i)) field
       mapM_ mkField $ zip locs [0..size-1]

       --comment Exit "RecordCreation"
       return record
              
translate' (ArrayCreation size init _) = 
    do --comment Enter "ArrayCreation"

       size <- translate' size  
       mips_array_size <- mem
       init <- translate' init
       i <- mem
       array <- mem
       cond <- label "create_array_cond"
       body <- label "create_array_body"
       end <- label "create_array_end"
       
       addInstr $ BinopInstr PLUS mips_array_size size (Const 1)
       addInstr $ Malloc mips_array_size array
       -- for i=0, i<size, array[i]=val, i++
       addInstr $ Move (MemOffset array (Const 0)) size -- mips_array[0]=size

       addInstr $ Move i (Const 1) -- i=0 i.e. 1
       addLabel cond
       addInstr $ Cjump LT i mips_array_size body end -- i<size i.e. size+1
       addLabel body
       addInstr $ Move (MemOffset array i) init -- array[i]=val
       i <- inc i -- i++
       addInstr $ Jump cond
       addLabel end

       --comment Exit "ArrayCreation"
       return array

translate' (For identifier low high body _) = 
    do --comment Enter "For"

       array <- mem
       offset <- mem
       size <- mem

       array_cond <- label "for_array_cond"
       array_body <- label "for_array_body"
       array_end <- label "for_array_end"

       for_cond <- label "for_cond"
       for_body <- label "for_body"
       for_end <- label "for_end"

       let id = toMem identifier
       low <- translate' low
       high <- translate' high
       
       addInstr $ BinopInstr MINUS size high low 
       size <- inc size -- b/c range is inclusive

       pushBreakpoint for_end

       -- make an array of [low..high]                
       addInstr $ Malloc size array
       addInstr $ Move offset (Const 0)
       addLabel array_cond
       addInstr $ Cjump GT low high array_end array_body
       addLabel array_body
       addInstr $ Move (MemOffset array offset) low
       low <- inc low
       offset <- inc offset
       addInstr $ Jump array_cond
       addLabel array_end

       addInstr $ Move offset (Const 0)
       addLabel for_cond                
       addInstr $ Cjump LT offset size for_body for_end
       addLabel for_body
       addInstr $ Move id (MemOffset array offset)
       translate' body
       offset <- inc offset
       addInstr $ Jump for_cond
       addLabel for_end

       popBreakpoint

       --comment Exit "For"
       return void

translate' (While cond body _) = 
    do --comment Enter "While"

       while_cond <- label "while_cond"
       while_body <- label "while_body"
       while_end <- label "while_end"

       pushBreakpoint while_end
       
       addLabel while_cond
       c <- translate' cond
       addInstr $ Cjump EQ c (Const 0) while_end while_body

       addLabel while_body
       b <- translate' body
       addInstr $ Jump while_cond

       addLabel while_end

       popBreakpoint      

       --comment Exit "While"
       return void

translate' (If c t f _) = 
    do --comment Enter "If"

       true_branch <- label "if_true"
       false_branch <- label "if_false"
       end <- label "if_end"
       ret <- mem
      
       c <- translate' c
       -- hmm, what are bools? 0 & 1? does '2<7' return '1'
       addInstr $ Cjump EQ c (Const 1) true_branch false_branch 

       addLabel true_branch
       t <- translate' t
       addInstr $ Move ret t  -- unifies return value
       addInstr $ Jump end
 
       addLabel false_branch
       f <- translate' f
       addInstr $ Move ret f  -- unifies return value
       addInstr $ Jump end

       addLabel end

       --comment Exit "If"
       return ret

translate' (Funcall name args _) = 
    do --comment Enter "Funcall"

       ret <- mem
       f <- translate' name
       args <- mapM translate' args
       addInstr $ Call f args ret 
       
       --comment Exit "Funcall"
       return ret

------------------------
-- testing
x = Unescaped "g"
i = Id x

t = translate

ai = ArrayRef (Id $ Unescaped "a;1") (Binop Add (Num 1) $ (Id $ Unescaped "i;2")) []
aij =  ArrayRef ai (Id $ Unescaped "j;3") []
stub = Assign aij (Num 1) [] 

addition = Binop Add (Num 1) (Num 2)

assignment = Assign i i []

