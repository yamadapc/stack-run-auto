module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM_)
import           Data.List.Utils          (uniq)
import           Development.FileModules
import qualified STMContainers.Set        as Set
import           System.Environment       (getArgs)

main :: IO ()
main = do
    args <- getArgs
    seen <- Set.newIO
    modules <- mapConcurrently (fileModulesRecur seen) args
    forM_ (uniq (concat modules)) putStrLn
