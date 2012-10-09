module Language.While.Interpreter where
import Language.While.Syntax
import Data.Map
import Data.Maybe
import Prelude hiding (LT,GT,EQ, lookup)

--Program state
data Env a = Env {
                                  store :: Map Var a,
                                  output :: [a]
                 } deriving (Show)

new_env :: Env a
new_env = Env {store=empty, output=[]}

--State Reads and Writes
update_store :: (Integral a) => Env a -> Var -> a -> Env a
update_store env var e = let sto = store env 
                            in env {store=insert var e sto}

store_lookup :: (Integral a) => Env a -> (Var -> AExpr a)
store_lookup env = \var -> let sto = (store env) in
                             if member var sto
                                then Const $ fromJust $ lookup var (store env)
                                else error ("Unbound varible used: " ++ (unvar var))

write :: (Integral a) => Env a -> a -> Env a
write e i = let o = output e
              in e {output=o++[i]}

--run and evaluation 
run :: Integral a => Env a -> Stmt a -> Env a
run phi Skip = phi
run phi (Assign v e)  = update_store phi v (eval (store_lookup phi) e)
run phi (Puts i) = write phi (eval (store_lookup phi) i)
run phi (If b1 s1 s2)   = if (evalb (store_lookup phi) b1)
                             then run phi s1
                             else run phi s2
run phi w@(While b1 s1) = if (evalb (store_lookup phi) b1)
                           then run phi (Seq (s1:w:[]))
                           else phi
run phi (Seq []) = phi
run phi (Seq (s1:ss)) = run (run phi s1) (Seq ss)


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
