module Codegen where
import Prelude hiding (GT,EQ,LT)
import IRSig
import Data.IORef
import Data.Char(ord)
import Data.List(isPrefixOf)
import Control.Monad
import qualified Data.Set as S

import qualified Data.HashTable as H

binopInstr :: BinopEnum -> String
binopInstr PLUS = "add"
binopInstr MUL = "mul"
binopInstr MINUS = "sub"
binopInstr DIV = "div"
binopInstr AND = "and"
binopInstr OR = "or"


relopInstr :: RelopEnum -> String
relopInstr LT = "slt"
relopInstr LE = "sle"
relopInstr EQ = "seq"
relopInstr NEQ = "sne"
relopInstr GT = "sgt"
relopInstr GE = "sge"

relopInstrJump :: RelopEnum -> String
relopInstrJump LT = "blt"
relopInstrJump LE = "ble"
relopInstrJump EQ = "beq"
relopInstrJump NEQ = "bne"
relopInstrJump GT = "bgt"
relopInstrJump GE = "bge"

-- the functions that make MIPS instructions
-- (all instructions should be created through these)

makeLabelInstr :: Label -> String
makeLabelInstr l = show l ++ ":"

makeTernaryInstr :: (Show a, Show b, Show c) => String -> a -> b -> c -> String
makeTernaryInstr op s1 s2 s3 = op ++ " " ++ show s1 ++ ", " ++ show s2 ++ ", " ++ show s3

makeJumpInstr :: Label -> String
makeJumpInstr l = "j " ++ show l


-- The three registers we are using for intermediate computation
data Register = T0
              | T1
              | T2
              | V0
              | A0
              | A1
              | A2
              | A3
              | A4
              | A5
              | A6
              | A7
   deriving (Eq, Ord, Enum)


instance Show Register where
    show T0 = "$t0"
    show T1 = "$t1"
    show T2 = "$t2"
    show V0 = "$v0"
    show A0 = "$a0"
    show A1 = "$a1"
    show A2 = "$a2"
    show A3 = "$a3"
    show A4 = "$a4"
    show A5 = "$a5"
    show A6 = "$a6"
    show A7 = "$a7"

{-

 -- Semantics of Move: Dest and Src are in memory
 -- (but with a Src==Const, it's a load immediate)
 -- that is, they are pointers
 -- move the value pointed to by src into the location pointed to by dest
 | Call Fun [Arg] Return
 | MakeClosure Pointer Label
-}
codegen :: [TAC] -> [FunDef] -> IO ()
codegen instrs fs = do memory <- H.new (==) H.hashString
                       H.insert memory "x;1" 7 -- DUMMY VALUE
                       let codegen' :: TAC -> IO ()
                           codegen' (LabelDecl l) = write $ makeLabelInstr l
                           codegen' (Comment Enter s) = write $ "# (" ++ s
                           codegen' (Comment Exit  s) = write $ "# " ++ s ++ ")"
                           codegen' (Malloc size dest) = do moveToReg size A0
                                                            write $ "sub $sp, $sp, 16"
                                                            write $ "jal malloc"
                                                            write $ "add $sp, $sp, 16"
                                                            -- now v0 holds the pointer
                                                            moveToVal dest V0
                           codegen' (AllocateString str dest) = do let size = length str + 5 -- number of bytes to allocate
                                                                   write $ "li $a0, " ++ show size
                                                                   write $ "sub $sp, $sp, 16"
                                                                   write $ "jal malloc"
                                                                   write $ "add $sp, $sp, 16"
                                                                   let ascii_string = map ord str ++ [0]
                                                                   write $ "li $t0, " ++ show (length str)
                                                                   write $ "sw $t0, ($v0)"
                                                                   forM (zip [4..] ascii_string) $ \(offset, chr) -> do
                                                                     write $ "li $t0, " ++ show chr
                                                                     write $ "sb $t0, " ++ show offset ++ "($v0)"
                                                                   moveToReg dest V0
        
                           codegen' (BinopInstr op d s1 s2) = do moveToReg s1 T1
                                                                 moveToReg s2 T2
                                                                 write $ makeTernaryInstr (binopInstr op) T0 T1 T2
                                                                 moveToVal d T0
        
                           codegen' (Relop op d s1 s2) = do moveToReg s1 T1
                                                            moveToReg s2 T2
                                                            write $ makeTernaryInstr (relopInstr op) T0 T1 T2
                                                            moveToVal d T0
                                                                  
                           codegen' (Move dest src) = do moveToReg src  T0
                                                         moveToVal dest T0
        
                           codegen' (Jump l) = write $ makeJumpInstr l
                           codegen' (Cjump op s1 s2 l1 l2) = do moveToReg s1 T0
                                                                moveToReg s2 T1
                                                                write $ makeTernaryInstr (relopInstrJump op) T0 T1 l1
                                                                write $ "j " ++ show l2
        
                           codegen' (Call fun args ret) = do moveToReg fun T0          -- the closure obj
                                                             write $ "lw $v0, ($t0)"   -- the closure's parent environment
                                                             write $ "lw $t0, 4($t0)"  -- the address to jump to
                                                             -- put the first 8 args in the a register
                                                             let first_args = take 8 args
                                                             forM_ (zip first_args [A0 .. A7]) $ \(arg, reg) ->
                                                                 moveToReg arg reg
                                                             -- push all of the args on the stack (backwards)
                                                             write $ "sub $sp, $sp, " ++ show (4 * length args)
                                                             forM_ (zip (reverse args) [4,8..]) $ \(arg, offset) -> do
                                                                 moveToReg arg T1 -- $t0 is in use
                                                                 write $ "sw $t1, " ++ show offset ++ "($sp)"
                                                             -- call the function
                                                             write $ "jalr $t0"
                                                             write $ "add $sp, $sp, " ++ show (4 * length args)
                                                             -- write the returned value to the right place
                                                             moveToVal ret V0
        
                           codegen' (MakeClosure dest label) = do write $ "li $a0, 8" -- malloc 2 words
                                                                  write $ "sub $sp, $sp, 16"
                                                                  write $ "jal malloc"
                                                                  write $ "add $sp, $sp, 16"
                                                                  -- v0 now holds the pointer to the closure object
                                                                  moveToVal dest V0
                                                                  -- store the current escaped variable record
                                                                  write $ "lw $t0, 4($fp)"
                                                                  write $ "sw $t0, ($v0)"
                                                                  -- store the label to jump to
                                                                  let Label labelstr = label
                                                                  if "builtin$" `isPrefixOf` labelstr
                                                                     then write $ "la $t0, a_" ++ drop 8 labelstr
                                                                     else write $ "la $t0, " ++ show label
                                                                  write $ "sw $t0, 4($v0)"
        

                           codegen' (Error kind)  = do let code = case kind of
                                                                    Array_out_of_bounds -> 1
                                                                    Nil_pointer_dereference -> 2
                                                                    Nil_pointer_assignment -> 3
                                                       write $ "li $a0, " ++ show code
                           codegen' (Return src) = moveToReg src V0

        
                           write = putStrLn
                           
                           moveToReg :: Val -> Register -> IO ()
                           moveToReg (Mem id) reg = do mem <- H.lookup memory id
                                                       case mem of
                                                         Nothing -> fail $ "codegen: id " ++ show id ++ " not yet allocated"
                                                         Just offset -> write $ "lw " ++ show reg ++ ", " ++ show offset ++ "($fp)"
                           moveToReg (MemScope id scope) reg = do mem <- H.lookup memory id
                                                                  case mem of
                                                                    Nothing -> fail $ "codegen: id " ++ show id ++ " not yet allocated"
                                                                    Just offset -> do write $ "lw $t3, 4($fp)"
                                                                                      replicateM scope $ write $ "lw $t3, ($t3)" -- go up one scope
                                                                                      write $ "lw " ++ show reg ++ ", " ++ show offset ++ "($t3)" -- index into the EVR
        
        
                           moveToReg (MemOffset v1 v2) reg = do moveToReg v1 T1
                                                                moveToReg v2 T2
                                                                write $ "move $t6, $t1"
                                                                write $ "move $t5, $t2"
                                                                write $ "add $t4, $t5, $t6"
                                                                write $ "lw " ++ show reg ++ ", ($t4)"

                           moveToReg (Const num) reg = write $ "li " ++ show reg ++ ", " ++ show num
                                 
                           moveToVal :: Val -> Register -> IO ()
                           moveToVal (Mem id) reg = do mem <- H.lookup memory id
                                                       case mem of
                                                         Nothing -> fail $ "codegen: id " ++ show id ++ " not yet allocated"
                                                         Just offset -> write $ "sw " ++ show reg ++ ", " ++ show offset ++ "($fp)"
        
                           moveToVal (MemScope id scope) reg = do mem <- H.lookup memory id
                                                                  case mem of
                                                                    Nothing -> fail $ "codegen: id " ++ show id ++ " not yet allocated"
                                                                    Just offset -> do write $ "lw $t3, 4($fp)"
                                                                                      replicateM scope $ write $ "lw $t3, ($t3)" -- go up one scope
                                                                                      write $ "sw " ++ show reg ++ ", " ++ show offset ++ "($t3)" -- index into the EVR
                           moveToVal (MemOffset v1 v2) reg = do moveToReg v1 T1
                                                                moveToReg v2 T2
                                                                write $ "move $t6, $t1"
                                                                write $ "move $t5, $t2"
                                                                write $ "add $t4, $t5, $t6"
                                                                write $ "sw " ++ show reg ++ ", ($t4)"

                           moveToVal (Const _) _ = fail $ "codegen: attempt to write into Const"
        
                           unescaped (Mem _) = True
                           unescaped _       = False
                                                                                             
                           escaped (MemScope _ 0) = True
                           escaped _              = False
        
                           extract_unescaped (Malloc v1 v2) = filter unescaped [v1,v2]
                           extract_unescaped (AllocateString _ v) = filter unescaped [v]
                           extract_unescaped (BinopInstr _ d s1 s2) = filter unescaped [d,s1,s2]
                           extract_unescaped (Relop      _ d s1 s2) = filter unescaped [d,s1,s2]
                           extract_unescaped (Cjump _ s1 s2 _ _) = filter unescaped [s1,s2]
                           extract_unescaped (Move d s) = filter unescaped [d,s]
                           extract_unescaped (Call f args r) = filter unescaped $ [f,r] ++ args
                           extract_unescaped (MakeClosure ptr _) = filter unescaped [ptr]
                           extract_unescaped (Return v) = filter unescaped [v]
                           extract_unescaped _ = []
        
                           extract_escaped (Malloc v1 v2) = filter escaped [v1,v2]
                           extract_escaped (AllocateString _ v) = filter escaped []
                           extract_escaped (BinopInstr _ d s1 s2) = filter escaped [d,s1,s2]
                           extract_escaped (Relop      _ d s1 s2) = filter escaped [d,s1,s2]
                           extract_escaped (Cjump _ s1 s2 _ _) = filter escaped [s1,s2]
                           extract_escaped (Move d s) = filter escaped [d,s]
                           extract_escaped (Call f args r) = filter escaped $ [f,r] ++ args
                           extract_escaped (MakeClosure ptr _) = filter escaped [ptr]
                           extract_escaped (Return v) = filter escaped [v]
                           extract_escaped _ = []
        
                           initializeFunDef (FunDef lbl args instrs) = let unescaped_mems = S.fromList $ filter unescaped args ++ concatMap extract_unescaped instrs
                                                                           escaped_mems = S.fromList $ filter escaped args ++ concatMap extract_escaped instrs
                                                                       in do forM (zip (S.elems escaped_mems) [4,8..]) $ \(MemScope id 0, offset) ->
                                                                                 H.insert memory id offset
                                                                       -- 4($fp) is the pointer to the escaped value record
                                                                       -- 8($fp) is the old frame pointer
                                                                       -- 12($fp) is the return address
                                                                             forM (zip (S.elems unescaped_mems) [16,20..]) $ \(Mem id, offset) ->
                                                                                 H.insert memory id offset
        
                           processFunDef (FunDef lbl args instrs) = do let unescaped_mems = S.fromList $ filter unescaped args ++ concatMap extract_unescaped instrs
                                                                           escaped_mems = S.fromList $ filter escaped args ++ concatMap extract_escaped instrs
                                                                       write $ makeLabelInstr lbl
                                                                       -- Allocate stack space
                                                                       write $ "sub $sp, $sp, " ++ show (12 + 4 * S.size unescaped_mems)
                                                                       write $ "sw $ra, 12($sp)"
                                                                       write $ "sw $fp, 8($sp)"
                                                                       write $ "sw $v0, 4($sp)" -- "hide" the EVR pointer from jal
                                                                       write $ "move $fp, $sp"
                                                                       -- Allocate space for escaped variable record 
                                                                       write $ "li $a0, " ++ show (4 + 4 * S.size escaped_mems)
                                                                       write $ "sub $sp, $sp, 16"
                                                                       write $ "jal malloc"
                                                                       write $ "add $sp, $sp, 16"
                                                                             
                                                                       write $ "lw $t0, 4($fp)" -- t0 now has the pointer
                                                                       write $ "sw $v0, 4($fp)" -- 4($fp) now has the EVR
                                                                       write $ "sw $t0, ($v0)"  -- the first element of the EVR is the parent pointer
           
                                                                       forM (zip args [12 + 4 * S.size unescaped_mems, 16 + 4 * S.size unescaped_mems ..]) $ \(arg, off) -> do
                                                                         write $ "lw $t1, " ++ show off ++ "($sp)"
                                                                         moveToVal arg T1
                                                                             
                                                                       mapM codegen' instrs  -- actually write out the code
   
                                                                       -- Deallocate stack space        
                                                                       write $ "lw $ra, 12($sp)"
                                                                       write $ "lw $fp, 8($sp)"
                                                                       write $ "add $sp, $sp, " ++ show (12 + 4 * S.size unescaped_mems)
                                                                       write $ "jr $ra"
                       -- Main label
                       -- Prepend header file:
                       header <- readFile "lib/stdlib.s" -- yuck!
                       write $ header
                       write $ "main:"
                       
        
                       mapM_ initializeFunDef fs
        
                       let unescaped_mems = S.fromList $ concatMap extract_unescaped instrs
                           escaped_mems = S.fromList $ concatMap extract_escaped instrs
                       forM (zip (S.elems escaped_mems) [4,8..]) $ \(MemScope id 0, offset) ->
                           H.insert memory id offset
                       -- 4($fp) is the pointer to the escaped value record
                       -- 8($fp) is the old frame pointer
                       -- 12($fp) is the return address
                       forM (zip (S.elems unescaped_mems) [16,20..]) $ \(Mem id, offset) ->
                           H.insert memory id offset

                       write $ "sub $sp, $sp, " ++ show (12 + 4 * S.size unescaped_mems)
                       write $ "sw $ra, 12($sp)"
                       write $ "sw $fp, 8($sp)"
                       write $ "move $fp, $sp"
                       -- Allocate space for escaped variable record 
                       write $ "li $a0, " ++ show (4 + 4 * S.size escaped_mems)
                       write $ "sub $sp, $sp, 16"
                       write $ "jal malloc"
                       write $ "add $sp, $sp, 16"
                       
                       write $ "sw $v0, 4($fp)" -- 4($fp) now has the EVR

                       mapM_ codegen' instrs


                       -- Deallocate stack space
                       write $ "lw $ra, 12($sp)"
                       write $ "lw $fp, 8($sp)"
                       write $ "add $sp, $sp, " ++ show (12 + 4 * S.size unescaped_mems)

                       write $ "li $a0, 0"
                       write $ "jal a_exit"

                       -- Output function bodies
                       mapM_ processFunDef fs

