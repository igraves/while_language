module Language.While.Interpreter where
import Language.While.Syntax
import Data.Map
import Data.Maybe
import Prelude hiding (LT,GT,EQ, lookup)

--Program state
data Env a = Env {
                                  store :: Map Var (AExpr a)
                               }  -- Have fun showing this.

--State Reads and Writes
update_store :: (Integral a) => Env a -> Var -> AExpr a -> Env a
update_store env var exp = let sto = store env 
                            in env {store=insert var exp sto}

store_lookup :: (Integral a) => Env a -> (Var -> AExpr a)
store_lookup env = \var -> let sto = (store env) in
                             if member var sto
                                then fromJust $ lookup var (store env)
                                else error ("Unbound varible used: " ++ (unvar var))


--run and evaluation 
run :: Integral a => Env a -> Stmt a -> Env a
run phi Skip = phi
run phi (Assign v exp)  = update_store phi v exp
run phi (If b1 s1 s2)   = if (evalb (store_lookup phi) b1)
                             then run phi s1
                             else run phi s2
run phi w@(While b1 s1) = if (evalb (store_lookup phi) b1)
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
