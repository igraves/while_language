module Language.While.Interpreter where
import Language.While.Syntax
import Prelude hiding (LT,GT,EQ)

data Env a = Env {
                     store :: Var -> AExpr a
                 }  -- Have fun showing this.

--Don't Judge me
create_env :: Integral a => [(String, a)] -> Env a
create_env = create_env_h Env {store = \x -> error "Unbound variable."}
  where
      create_env_h e [] = e 
      create_env_h e ((x,v):xs) = create_env_h (Env (tweak (Var x) 
                                                           (Const v) 
                                                           (store e))) xs
--See the prior comment
tweak :: Var -> a -> (Var -> a) -> (Var -> a)
tweak i v f = (\x -> if x == i then v else f x)

update_store i v f = tweak i v f

run :: Integral a => Env a -> Stmt a -> Env a
run phi Skip = phi
run phi (If b1 s1 s2)   = if (evalb (store phi) b1)
                             then run phi s1
                             else run phi s2
run phi w@(While b1 s1) = if (evalb (store phi) b1)
                           then run phi (Seq s1 w)
                           else phi
run phi (Seq s1 s2) = run (run phi s1) s2


eval ::   Integral a => (Var -> AExpr a) -> AExpr a -> a
eval _   (Const a)   = a
eval sto (Ident i)   = eval sto (sto i)
eval sto (Neg e1)    = negate $ eval sto e1
eval sto (Add e1 e2) = (eval sto e1) + (eval sto e2)
eval sto (Sub e1 e2) = (eval sto e1) - (eval sto e2)
eval sto (Mul e1 e2) = (eval sto e1) * (eval sto e2)
eval sto (Div e1 e2) = (eval sto e1) `div` (eval sto e2) 

evalb :: Integral a => (Var -> AExpr a) -> BExpr a -> Bool
evalb sto (LT e1 e2)  =  (eval sto e1) < (eval sto e2)
evalb sto (LTE e1 e2) =  (eval sto e1) <= (eval sto e2)
evalb sto (GT e1 e2)  =  (eval sto e1) > (eval sto e2)
evalb sto (GTE e1 e2) =  (eval sto e1) >= (eval sto e2)
evalb sto (EQ e1 e2)  =  (eval sto e1) == (eval sto e2)
evalb sto (NE e1 e2)  =  (eval sto e1) /= (eval sto e2)
evalb sto (NOT b1)    =  not $ evalb sto b1
evalb _ F             = False
evalb _ T             = True
