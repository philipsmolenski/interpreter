-- Main module running the whole interpreter

module Main where

import Big.LexBig
import Big.ParBig
import Big.AbsBig
import Big.ErrM
import Big.PrintBig
import Big.SkelBig
import Interpreter
import TypeCheck
import System.Exit
import System.IO
import System.Environment

-- Run type checking first. If there is an error then quit with 
-- static error message. Else Run interpreter.
runTCMandInterpreter :: Program -> IO ()
runTCMandInterpreter prog = do
  ans1 <- runTCM $ runTCMProgram $ prog  
  case ans1 of 
    Left err -> putStrLn $ "STATIC ERROR: " ++ err
    Right _ -> do
      ans2 <- runInterpreterMonad $ runInterpreterProgram $ prog 
      case ans2 of 
        Left err -> putStrLn $ "DYNAMIC ERROR: " ++ err
        Right _ -> return () 

-- Run parser. If there is an error then quit with parse error.
-- Else run typechecker and iterpreter.
runAll :: String -> IO ()
runAll contents =  
  let x = pProgram (myLexer contents) in case x of 
    (Right s) -> do 
      putStrLn "Parsing OK"
      runTCMandInterpreter s
      exitSuccess 
    (Left s) -> do
      putStrLn "PARSE ERROR: "
      print s

main :: IO()
main = do
  args <- getArgs
  case args of 
    -- if no arguments provided interpret from command line
    [] -> do 
      contents <- getContents
      runAll contents 
    -- interpret program passed as argument 
    [x] -> do
      handle <- openFile x ReadMode
      contents <- hGetContents handle
      runAll contents
    _ -> putStrLn "ERROR: Bad number of arguments"