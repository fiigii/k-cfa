module Main where

import System.IO
import LabeledAst
import Parser
import ControlFlow

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = withFile
       "test/fun.js"
       ReadMode
       (\handle -> do
           source <- hGetContents handle
           let ((cs, rs), ast) = interpreter source
               c = Map.toList cs
               r = Map.toList rs
           putStrLn "Pragram: "
           putStrLn $ show ast
           putStrLn "\nControl Flow:"
           mapM_ (\((Cache n ctx), v) -> putStrLn $ show n ++ " at " ++ show ctx ++ " : " ++ show (Set.toList v)) c
           mapM_ (\((Envir n ctx), v) -> putStrLn $ show n ++ " at " ++ show ctx ++ " : " ++ show (Set.toList v)) r
       )

interpreter source = case jsparse source of
                      Right ts -> (controlFlow ts, fst $ convert ts)
                      Left e -> error $ show e               
