module Main where

import System.IO
import LabeledAst
import Parser
import ControlFlow

import qualified Data.Map as Map

main :: IO ()
main = withFile
       "test/fun.js"
       ReadMode
       (\handle -> do
           source <- hGetContents handle
           -- putStrLn $ show $ jsparse source
           let ((c, r), ast) = interpreter source
           putStrLn $ "\nLabeled Program\n" ++ show ast
           putStrLn "\nCache"
           mapM_ (\(k, v) -> putStrLn $ show k ++ " -> " ++ show v) $ Map.toList c
           putStrLn "\nEnvir"
           mapM_ (\(k, v) -> putStrLn $ k ++ " -> " ++ show v)  $ Map.toList r
       )
--interpreter :: String -> a
interpreter source = case jsparse source of
                      Right ts -> (controlFlow ts, fst $ convert ts)
                      Left e -> error $ show e               
