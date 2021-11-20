module Main where

import Lib
import Parsers
import System.Environment

type Flag = String

main = do
    args <- getArgs
    -- flags <- getFlags
    putStrLn "The arguements are: "
    mapM putStrLn args
