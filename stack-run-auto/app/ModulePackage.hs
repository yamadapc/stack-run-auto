module Main where

import           Control.Concurrent.Async
import           Control.Monad            (void, (>=>))
import           StackRunAuto
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    void $ mapConcurrently (modulePackage >=> putStrLn) args
