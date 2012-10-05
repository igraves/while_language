module Language.While.Syntax where


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

data Stmt a =  Skip
                              | Assign Var (AExpr a)
                              | If (BExpr a) (Stmt a) (Stmt a) -- Then s1 else s2 respective
                              | While (BExpr a) (Stmt a)
                              | Puts (AExpr a)
                              | Seq [Stmt a] deriving (Show, Eq)-- Left, then right
