module Language.While.Compiler where

import Data.Word
import Data.Int
import Data.Set hiding (foldr)
import Language.While.Syntax
import LLVM.Core
import LLVM.ExecutionEngine
import Prelude hiding (LT, GT, EQ)

bldVarargs :: CodeGenModule (Function (Int32 -> IO ()))
bldVarargs =
   withStringNul "A number %d\n" (\fmt2 -> do
      printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Int32)
      func <- createFunction ExternalLinkage $ \ x -> do
        tmp2 <- getElementPtr0 fmt2 (0::Int32, ())
        let p2 = castVarArgs printf :: Function (Ptr Word8 -> Int32 -> IO Int32)
        _ <- call p2 tmp2 x
        ret ()
      return func
   )


boilerplate :: Stmt a -> CodeGenModule (Function (IO ()))
boilerplate stmts = do
                      let vars = toList $ crawlStmt stmts empty
                      error "Incomplete" 


crawlStmt :: Stmt a -> Set String -> Set String
crawlStmt (Assign (Var v) e1) s  = crawlAExp e1 $ insert v s
crawlStmt (If b1 s1 s2) s = crawlStmt s2 $ crawlStmt s1 $ crawlBExp b1 s
crawlStmt (While b1 s1) s = crawlStmt s1 $ crawlBExp b1 s
crawlStmt (Puts e1) s = crawlAExp e1 s
crawlStmt (Seq stmts) s = foldr (\y set -> crawlStmt y set) s stmts

crawlAExp :: AExpr a -> Set String -> Set String
crawlAExp (Ident (Var v)) s = insert v s
crawlAExp (Add e1 e2) s = crawlAExp e2 (crawlAExp e1 s)
crawlAExp (Sub e1 e2) s = crawlAExp e2 (crawlAExp e1 s)
crawlAExp (Mul e1 e2) s = crawlAExp e2 (crawlAExp e1 s)
crawlAExp (Div e1 e2) s = crawlAExp e2 (crawlAExp e1 s)
crawlAExp (Neg e1) s = (crawlAExp e1 s)
crawlAExp (Const c) s = s

crawlBExp :: BExpr a -> Set String -> Set String
crawlBExp (NOT b1) s = crawlBExp b1 s
crawlBExp (LT e1 e2)  s = crawlAExp e2 (crawlAExp e1 s)
crawlBExp (LTE e1 e2) s = crawlAExp e2 (crawlAExp e1 s)
crawlBExp (GT e1 e2)  s = crawlAExp e2 (crawlAExp e1 s)
crawlBExp (GTE e1 e2) s = crawlAExp e2 (crawlAExp e1 s)
crawlBExp (EQ e1 e2)  s = crawlAExp e2 (crawlAExp e1 s)
crawlBExp (NE e1 e2)  s = crawlAExp e2 (crawlAExp e1 s)
