module Main where

import Data.Monoid
import Terms
import LambdaParser



printParser :: String -> IO ()
printParser s = do
  let term = parseLambda s
  case term of
    Just v -> print $ eval v
    _      -> putStrLn $ "Could not parse: " <> s
  

runLoop :: IO ()
runLoop  = do
  s <- getLine
  printParser s
  runLoop
  
main :: IO ()
main = do
  putStrLn "Welcome to a basic lambda interpreter."
  runLoop

