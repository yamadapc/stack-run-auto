module Main where

import           Control.Concurrent.Async
import           Control.Monad            (forM_)
import           Data.Maybe               (catMaybes)
import           StackRunAuto
import           System.Environment
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    pkgs <- mapConcurrently modulePackage args
    forM_ (catMaybes pkgs) putStrLn
