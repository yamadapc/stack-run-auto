module Main where

import           StackRunAuto
import           System.Environment (getArgs)
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    case args of
        [] -> error "Usage: stack-run-auto <file>"
        (fname:_) -> run (Options fname)
