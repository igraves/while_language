module Language.While.Syntax where


data Var = Var String

data (Num a, Ord a) => AExpr a =  Add (AExpr a) (AExpr a) 
                                | Sub (AExpr a) (AExpr a)
                                | Mul (AExpr a) (AExpr a)
                                | Div (AExpr a) (AExpr a)
                                | Neg (AExpr a)
                                | Ident Var
                                | Const a 
                                -- Missing is a parens case

data (Num a, Ord a) => BExpr a =  T
                                | F
                                | LT  (AExpr a) (AExpr a)
                                | LTE (AExpr a) (AExpr a)
                                | GT  (AExpr a) (AExpr a)
                                | GTE (AExpr a) (AExpr a)
                                | EQ  (AExpr a) (AExpr a) 
                                | NE  (AExpr a) (AExpr a)
                                | NOT (BExpr a) 
                                -- Missing is a parens case

data (Num a, Ord a) => Stmt a =  Skip
                              | Assignment Var (AExpr a)
                              | If (BExpr a) (Stmt a) (Stmt a) -- Then s1 else s2 respective
                              | While (BExpr a) (Stmt a)
                              | Call Var (AExpr a)
                              | Seq (Stmt a) (Stmt a) -- Left, then right

