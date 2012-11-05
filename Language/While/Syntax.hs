module Language.While.Syntax where
import Prelude hiding (GT,LT,EQ)


data Var = Var String deriving (Show, Eq, Ord)
unvar :: Var -> String
unvar (Var s) = s

data AExpr a =  Add (AExpr a) (AExpr a) --1st + 2nd
                                | Sub (AExpr a) (AExpr a) --1st - 2nd
                                | Mul (AExpr a) (AExpr a) --1st * 2nd
                                | Div (AExpr a) (AExpr a) --1st/2nd
                                | Neg (AExpr a)
                                | Ident Var
                                | Const a deriving (Show, Eq)
                                -- Missing is a parens case

instance Functor AExpr where
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
  fmap f (Sub e1 e2) = Sub (fmap f e1) (fmap f e2)
  fmap f (Mul e1 e2) = Mul (fmap f e1) (fmap f e2)
  fmap f (Div e1 e2) = Div (fmap f e1) (fmap f e2)
  fmap f (Neg e1)    = Neg (fmap f e1)
  fmap _ (Ident v)   = (Ident v) 
  fmap f (Const c)   = Const (f c)

data BExpr a =  T
                                | F
                                | LT  (AExpr a) (AExpr a)
                                | LTE (AExpr a) (AExpr a)
                                | GT  (AExpr a) (AExpr a)
                                | GTE (AExpr a) (AExpr a)
                                | EQ  (AExpr a) (AExpr a) 
                                | NE  (AExpr a) (AExpr a)
                                | NOT (BExpr a) deriving (Show, Eq)
                                -- Missing is a parens case
instance Functor BExpr where
  fmap _ T           = T
  fmap _ F           = F
  fmap f (LT e1 e2)  = LT (fmap f e1) (fmap f e2)
  fmap f (LTE e1 e2) = LTE (fmap f e1) (fmap f e2)
  fmap f (GT e1 e2)  = GT (fmap f e1) (fmap f e2)
  fmap f (GTE e1 e2) = GTE (fmap f e1) (fmap f e2)
  fmap f (EQ e1 e2)  = EQ (fmap f e1) (fmap f e2)
  fmap f (NE e1 e2)  = NE (fmap f e1) (fmap f e2)
  fmap f be1         = fmap f be1

reduceBexpr :: BExpr a -> BExpr a
reduceBexpr (NOT x) = case x of
                           F -> T
                           T -> F
                           (LT y z)  -> GTE y z
                           (LTE y z) -> GT y z
                           (GT y z)  -> LTE y z
                           (GTE y z) -> LT y z
                           (EQ y z)  -> NE y z
                           (NE y z)  -> EQ y z
                           (NOT y)   -> reduceBexpr y
reduceBexpr x = x

--The Syntax Tree for programs
data Stmt a =  Skip
                              | Assign Var (AExpr a)
                              | If (BExpr a) (Stmt a) (Stmt a) -- Then s1 else s2 respective
                              | While (BExpr a) (Stmt a)
                              | Puts (AExpr a)
                              | Seq [Stmt a] deriving (Show, Eq)-- Left, then right

instance Functor Stmt where
  fmap f (Assign v e1) = Assign v $ fmap f e1
  fmap f (If be1 e1 e2) = If (fmap f be1) (fmap f e1) (fmap f e2)
  fmap f (While be1 e1) = While (fmap f be1) (fmap f e1)
  fmap f (Puts e1) = Puts (fmap f e1)
  fmap f (Seq ss) = Seq (map (fmap f) ss)

