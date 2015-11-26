module Main where

import           StackRunAuto
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "Usage: stack-run-auto <file>"
        (fname:_) -> run (Options fname)
