module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM_)
import           Data.List.Utils          (uniq)
import           Development.FileModules
import           System.Environment       (getArgs)
import           System.Exit              (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
      "--version":_ -> putStrLn "0.1.2.1"
      "--help":_ -> printUsage
      [] -> printUsage >> exitFailure
      ms -> do
        modules <- mapConcurrently fileModulesRecur ms
        forM_ (uniq (concat modules)) putStrLn
  where
    printUsage = putStrLn "Usage: file-modules <files...>"
