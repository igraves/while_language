module Main where

import Text.Parsec.String
import Language.While.Parser
import Language.While.Interpreter
import System.Environment


main = do
          a <- getArgs
          let fname = head a
          res <- parseFromFile stmts fname
          case (res) of
              Right res -> do
                             let res_env = run new_env res
                             putStrLn $ show $ output res_env
              Left err -> do 
                            putStrLn "Parse Error in file."
                            putStrLn $ show err
