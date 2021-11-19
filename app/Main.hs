module Main where

import Lib
import System.Environment

main = do
    args <- getArgs
    putStrLn "The arguements are: "
    mapM putStrLn args
