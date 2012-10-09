module Main where

import Text.Parsec.String
import Language.While.Parser
import Language.While.Interpreter
import System.Environment

main :: IO ()
main = do
          a <- getArgs
          let fname = head a
          r <- parseFromFile stmts fname
          case (r) of
              Right s -> do
                             let res_env = run new_env s 
                             putStrLn $ show $ output res_env
              Left err -> do 
                            putStrLn "Parse Error in file."
                            putStrLn $ show err
