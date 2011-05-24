module Type where

type Cmp = Int
type Inf = Bool
type ID = String


data Type = ArrayT Cmp Type
          | RecordT Cmp Inf [(ID, Type)]
          | FunctionT [Type] Type
          | NilT
          | NumT
          | StrT
          | VoidT
          | UnboundT ID
  deriving (Show, Ord)


instance Eq Type where
    ArrayT n _  == ArrayT m _ = n == m
    RecordT n _ _ == RecordT m _ _= n == m
    FunctionT args1 ret1 == FunctionT args2 ret2 = args1 == args2 && ret1 == ret2
    NilT == NilT = True
    NumT == NumT = True
    StrT == StrT = True
    VoidT == VoidT = True
    _ == _ = False