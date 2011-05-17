module Codegen where
import Control.Monad.Writer
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

makeTernaryInstr :: (Show a, Show b, Show c) => String -> a -> b > c -> String
makeTernaryInstr op s1 s2 s3 = op ++ " " ++ show s1 ++ " " ++ show s2 ++ " " ++ show s3

makeJumpInstr :: Label -> String
makeJumpInstr l = "j " ++ show l

-- The three registers we are using for intermediate computation
data Register = Reg0
              | Reg1
              | Reg2
deriving (Eq, Ord)


instance Show Register where
    show Reg0 = "$t0"
    show Reg1 = "$t1"
    show Reg2 = "$t2"


-- make this change in IR
--    type Label = IR
-- to
--    data Label = L ID
--    deriving (Eq, Ord)
--    instance Show Label where
--        show (L n) = "l" ++ show n

-- data ID = Inner Sym
--         | Outer Int Sym
--         | Escaped ??

codegen :: TAC -> IO ()
codegen t = do memory <- H.new (==) H.hashInt
               heapCounter <- newSTRef 0
               codegen' t
                   where codegen' :: TAC -> IO ()
                         codegen' (Label l) = write $ makeLabelInstr l
                         codegen' (Binop op d s1 s2) = do moveToReg s1 Reg1
                                                          moveToReg s2 Reg2
                                                          write $ makeMIPSInstr (binopInstr op) Reg0 Reg1 Reg2
                                                          moveToVal d Reg0
                         codegen' (Relop op d s1 s2) = do moveToReg s1 Reg1
                                                          moveToReg s2 Reg2
                                                          write $ makeMIPSInstr (relopInstr op) Reg0 Reg1 Reg2
                                                          moveToVal d Reg0
                                                          
                         codegen' (Jump l) = write $ makeJumpInstr l
                         codegen' (Cjump op s1 s2 l1 l2) = do moveToReg s1 Reg0
                                                              moveToReg s2 Reg1
                                                              write $ makeMIPSInstr (relopInstrJump op) Reg0 Reg1 l1
                                                              write $ "j " ++ show l2
                         write = putStrLn
                                 
                         moveToReg :: Val -> Register -> IO ()
                         moveToReg (Mem id) reg = do mem <- lookup memory id
                                                     case mem of
                                                       Nothing -> fail "codegen: id " ++ show id ++ " not yet allocated"
                                                       Just (StackValue offset) -> do write $ "lw " ++ show reg ++ " " 
                                                                                                ++ show offset ++ "($fp)"
                                                       Just (HeapValue) -> return () -- Activation records
                         moveToReg (MemOffset id off) reg = 0
                         moveToReg (Const num) = 0
                         


data MemoryValue = StackValue Int -- Offset from stack pointer
                 | HeapValue -- Activation records
