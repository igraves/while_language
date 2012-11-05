module Language.While.RAC where
import Control.Monad.Trans.State
import qualified Text.Parsec.String as PS
import Control.Monad.Trans
import Control.Monad
import Language.While.Syntax
import Prelude hiding (GT,LT,EQ)
import Language.While.Parser

--Return address code


data Register = Reg String  deriving (Show, Eq)
data StoreData = N IntStore | L Label deriving (Show, Eq)
type Label = String
type IntStore = Int
data Argument = R Register  | S StoreData deriving (Show, Eq)


addStore (N a) (N b) = N $ a + b
addStore _ _ = error "Cannot add number to label."

mulStore (N a) (N b) = N $ a * b
mulStore _ _ = error "Cannot mult number to label."

divStore (N a) (N b) = N $ a `div` b
divStore _ _ = error "Cannot div number to label."

negStore (N a) = N $ (- a)
negStore _  = error "Cannot negate label."

subStore (N a) (N b) = N $ a - b
subStore _ _ = error "Cannot add number to label."

cmpStore (N a) (N b) = a - b
cmpStore _ _ = error "Cannot compare numbers to label."

data Instruction =   MOV  Register Register           |
                     ADD  Register Register Register  |
                     MUL  Register Register Register  |
                     DIV  Register Register Register  |
                     NEG  Register Register           |
                     CMP  Register Register           |
                     SUB  Register Register Register  |
                     LOAD Register StoreData          |
                     PUTS Register                    |
                     RET    |
                     RETEQ  |
                     RETNEQ | RETLT  |
                     RETGT  |
                     RETLTE |
                     RETGTE |
                     POPR Register |
                     POP |
                     PUSH Argument deriving (Show, Eq)

type Frame = [Instruction]
type Entry = Label

data MachineState = MachineState {
                                    registers :: [(Register,StoreData)],
                                    zf :: Bool,
                                    sf :: Bool, -- True if negative
                                    stack :: [StoreData],
                                    frames :: [(Label,Frame)],
                                    cframe :: Frame
                                 } deriving Show

initialmachine = MachineState {
                                 registers=[],
                                 zf=False,
                                 sf=False,
                                 stack=[],
                                 frames=[],
                                 cframe=[]
                              }
initMachine :: [(Label,Frame)] -> Label -> [StoreData] -> MachineState
initMachine f l s = case lookup l f of
                          Just cf  -> initialmachine {frames=f,cframe=cf,stack=s}
                          Nothing  -> error "Bad initial frame given." 

type Machine = StateT MachineState IO

popstack :: Machine StoreData 
popstack = do
             s <- get 
             let (x:xs) = stack s
             put $ s {stack=xs}
             return x

ret :: Machine ()
ret = do
        s <- get
        if (length $ stack s) > 0 
          then do
                  p <- popstack 
                  s <- get
                  case p of
                      (L l) -> do
                                 let newframe = lookup l $ frames s
                                 case newframe of
                                       Just f -> put s {cframe=f}
                                       _ -> error "Label to nonexistent frame."
                      _ -> error "Number popped where label was expected."
          else  do
                  put s{cframe=[]} 
                  lift $ putStrLn "Program is halting on a empty stacks."
        return ()

advance :: Machine ()
advance = do
            s <- get
            let (f:fs) = cframe s
            put s {cframe=fs}
            return ()
        


putZF b = get >>= \x -> put x {zf=b}
putSF b = get >>= \x -> put x {sf=b}

getZF :: Machine Bool
getZF = get >>= \x -> return $ zf x

getSF :: Machine Bool
getSF = get >>= \x -> return $ sf x

lookupReg x ((r,v):rs) = if r == x
                            then v
                            else lookupReg x rs

updateReg x v [] = [(x,v)]
updateReg x v ((r,i):rs) = if x == r
                            then (x,v):rs
                            else (r,i) : updateReg x v rs

getReg r = get >>= \x -> return $ lookupReg r $ registers x

putReg r v = get >>= \x -> put x {registers=updateReg r v (registers x)}

exec :: Instruction -> Machine ()
exec (MOV dest src) = do
                              s <- getReg src
                              putReg dest s
                              advance

exec (ADD dest src1 src2) = do
                        r <- getReg src1 
                        s <- getReg src2
                        putReg dest $ addStore r s
                        advance

exec (MUL dest src1 src2) = do
                        r <- getReg src1 
                        s <- getReg src2
                        putReg dest $ mulStore r s
                        advance

exec (DIV dest src1 src2) = do
                        r <- getReg src1 
                        s <- getReg src2 
                        putReg dest $ divStore r s
                        advance

exec (NEG dest src) = do
                        r <- getReg src 
                        putReg dest $ negStore r 
                        advance

exec (CMP dest src) = do
                        r <- getReg dest
                        s <- getReg src
                        let res = cmpStore r s 
                        putZF $ res == 0
                        putSF $ res < 0
                        advance

exec (SUB dest src1 src2) = do 
                        r <- getReg src1
                        s <- getReg src2 
                        putReg dest $ subStore r s
                        advance

exec (LOAD dest arg) = putReg dest arg  >> advance
exec RET = ret 
exec RETEQ = do
                p <- getZF
                if p then ret else advance
exec RETNEQ = do
                p <- getZF
                if not p then ret else advance
exec RETLT = do
                z <- getZF
                s <- getSF
                if not z && s then ret else advance
exec RETGT = do
                z <- getZF
                s <- getSF
                if not z && not s then ret else advance
exec RETLTE = do
                z <- getZF
                s <- getSF
                if z || s then ret else advance
exec RETGTE = do
                z <- getZF
                s <- getSF
                if z || not s then ret else advance
exec (PUSH arg) = do
                    sto <- case arg of
                                (R reg) -> getReg reg
                                (S sto) -> return sto
                    s <- get
                    let stk = stack s
                    put s {stack=sto:stk}
                    advance
exec (PUTS arg) = do
                    lift (putStrLn $ show arg)
                    advance

run :: Machine ()
run = do
        s <- get
        let ins = head $ cframe s
        exec ins
        s <- get
        if (cframe s) == [] && (stack s) == [] -- The halting state
          then return ()
          else run 

{-
 - Test Program:
 - ra = 0
 - rb = 10
 - rc = 1
 - while ra < rb 
 -   ra = ra + rc
 - end
 -
 - succinctly:
 - while x < 10
 -   x += 1
 - end
 -}
test_frame_entry = [
                    LOAD (Reg "A") (N 0),
                    LOAD (Reg "B") (N 10),
                    LOAD (Reg "C") (N 1),
                    RET
                   ]
entry_frame = ("entry",test_frame_entry)

test_frame_body = [
                    CMP (Reg "A") (Reg "B"),
                    RETGTE, -- If not LT, quit looping
                    ADD (Reg "A") (Reg "A") (Reg "C"),
                    PUSH $ S $ L "body",
                    RET
                  ]
body_frame = ("body",test_frame_body) 
test_program_frames = entry_frame : body_frame : []
test_initial_stack = [L "body"]

testMachine = initMachine test_program_frames "entry" test_initial_stack


----
----Compilation procedures for while language to RAC
----
type Labeler = State (Int,[(String,Int)],[(Label,Frame)])

getLabel :: Labeler String
getLabel = do
             (x,s,z) <- get
             put ((x+1),s,z)
             return $ "x_" ++ (show x)
getVarLabelUpdate :: String -> Labeler String
getVarLabelUpdate var = do
                        (x,s,z) <- get
                        let r = lookup var s
                        case r of
                            Just num -> do
                                            put (x,replace var (num+1) s,z) 
                                            return $ var ++ "_" ++ (show (num+1))
                            Nothing  -> do 
                                            put (x,(var,1):s,z)
                                            return $ var ++ "_1"
              where
                  replace x v [] = [(x,v)]
                  replace x v ((i,j):xs) = if x == i then (x,v):xs else (i,j) : replace x v xs

getVarLabel v = do
                  (x,s,z) <- get
                  let r = lookup v s
                  case r of
                      Just num -> return $ v ++ "_" ++ (show num)
                      Nothing  -> return $ v ++ "_0" 

compileWA :: AExpr Int -> Labeler (String, [Instruction])
compileWA (Const i) = getLabel >>= \l -> return (l, [LOAD (Reg l) (N i)])
compileWA (Ident (Var i)) = getVarLabel i >>= \v -> return (v,[])
compileWA (Neg e1) = do
                        (res, ins) <- compileWA e1
                        x <- getLabel
                        return $ (res, ins ++ [NEG (Reg x) (Reg res)])
compileWA (Add e1 e2) = do
                         (res1, ins1) <- compileWA e1
                         (res2, ins2) <- compileWA e2
                         x <- getLabel
                         return (x, ins1 ++ ins2 ++ [ADD (Reg x) (Reg res1) (Reg res2)])
compileWA (Sub e1 e2) = do
                         (res1, ins1) <- compileWA e1
                         (res2, ins2) <- compileWA e2
                         x <- getLabel
                         return (x, ins1 ++ ins2 ++ [SUB (Reg x) (Reg res1) (Reg res2)])
compileWA (Mul e1 e2) = do
                         (res1, ins1) <- compileWA e1
                         (res2, ins2) <- compileWA e2
                         x <- getLabel
                         return (x, ins1 ++ ins2 ++ [MUL (Reg x) (Reg res1) (Reg res2)])
compileWA (Div e1 e2) = do
                         (res1, ins1) <- compileWA e1
                         (res2, ins2) <- compileWA e2
                         x <- getLabel
                         return (x, ins1 ++ ins2 ++ [DIV (Reg x) (Reg res1) (Reg res2)])
                          
testexpr = Add (Neg $ Const 1) (Const 2)
testcomp = runState (compileWA testexpr) (0,[],[])

newFrame :: Stmt Int -> Labeler String 
newFrame s@(Seq _) = do
                       (x,y,frames) <- get
                       ins <- compileWS s
                       let ins' = ins 
                       lbl <- getLabel 
                       put (x,y,((lbl,ins):frames))
                       return lbl
newFrame s = do
               (x,y,frames) <- get
               ins <- compileWS s
               let ins' = ins ++ [RET]
               lbl <- getLabel 
               put (x,y,((lbl,ins'):frames))
               return lbl
compileWS :: Stmt Int -> Labeler [Instruction]
compileWS (Assign (Var v) e1) = do
                                   (s1,ins1) <- compileWA e1 
                                   lbl <- getVarLabelUpdate v 
                                   return $ ins1 ++ [MOV (Reg lbl) (Reg s1)]

compileWS (If b s1 s2) = do
                            f1 <- newFrame s1 
                            f2 <- newFrame s2 -- Saving code space, but this will result in unreachable frames
                            case (reduceBexpr b) of
                                 T -> do
                                        let rlabel = PUSH $ S $ L f1
                                        return $ rlabel:[RET]
                                 F -> do
                                        let rlabel = PUSH $ S $ L f2
                                        return $ rlabel:[RET]
                                 x -> do
                                        let (y, z) = prjaexpr x
                                        (r1,ins1) <- compileWA y
                                        (r2,ins2) <- compileWA z
                                        let ins3 = [
                                                    PUSH $ S $ L f1,
                                                    CMP (Reg r1) (Reg r2),
                                                    wbtorb x,
                                                    POP, PUSH $ S $ L f2,
                                                    RET
                                                   ]
                                        return $ ins1 ++ ins2 ++ ins3

compileWS (While b s) = do
                          case (reduceBexpr b) of
                               F -> do
                                      return $ [] -- Just nothing.  Effectively a skip.
                               T -> do
                                      f <- newFrame s
                                      self <- getVarLabelUpdate "while" --Frame label
                                      (x,y,frames) <- get
                                      let jumpback = PUSH $ S $ L self
                                      let rlabel = PUSH $ S $ L f
                                      put (x,y,(self,jumpback:rlabel:[RET]):frames) --Always return to the loop body
                                      return $ jumpback:[RET] -- Jump to the while frame
                               c -> do
                                      f <- newFrame s
                                      self <- getVarLabelUpdate "while" --Frame label
                                      (x,y,frames) <- get
                                      let (y', z') = prjaexpr c
                                      (r1,ins1) <- compileWA y'
                                      (r2,ins2) <- compileWA z'

                                      let jumpback = PUSH $ S $ L self
                                      let rlabel = PUSH $ S $ L f
                                      let while_b = [
                                                      jumpback, --Push label back to while check 
                                                      rlabel, -- Push Label to the loop body
                                                      CMP (Reg r1) (Reg r2), -- check cmp
                                                      wbtorb c, -- Ret if true
                                                      POP, --If we didn't ret, pop label
                                                      RET -- Return to caller context
                                                    ]
                                      put (x,y,(self,while_b):frames)
                                      return $ ins1 ++ ins2 ++ jumpback:[RET]

compileWS (Seq (x:[])) = compileWS x
compileWS (Seq (x:xs)) = do
                          r <- compileWS x
                          rest <- compileWS $ Seq xs
                          return $ r ++ rest ++ [RET] --End sequences in a ret.
compileWS (Skip) = return []
compileWS (Puts e1) = do
                        (r1,ins1) <- compileWA e1
                        return $ ins1 ++ [PUTS (Reg r1)]
compileWS fail = error $ show fail


wbtorb :: BExpr Int -> Instruction
wbtorb (LT _ _ ) = RETLT
wbtorb (LTE _ _) = RETLTE
wbtorb (GT _ _ ) = RETGT
wbtorb (GTE _ _) = RETGTE
wbtorb (EQ _ _ ) = RETEQ
wbtorb (NE _ _) = RETNEQ

prjaexpr :: BExpr Int -> (AExpr Int, AExpr Int)
prjaexpr (LT x y ) = (x,y) 
prjaexpr (LTE x y) = (x,y) 
prjaexpr (GT x y ) = (x,y) 
prjaexpr (GTE x y) = (x,y) 
prjaexpr (EQ x y ) = (x,y) 
prjaexpr (NE x y ) = (x,y) 

compileWhile :: String -> IO ()
compileWhile fname = do
                        r <- PS.parseFromFile stmts fname
                        case (r) of
                            Right s   -> do
                                          (r,(_,_,s)) <- return $ runState (compileWS (fmap fromIntegral s)) (0,[],[]) 
                                          putStrLn $ "Top Level:"
                                          putStrLn $ show r
                                          putStrLn $ "Frames:"
                                          putStrLn $ show s
                            Left err -> do
                                          putStrLn "Parse error in file."
                                          putStrLn $ show err
