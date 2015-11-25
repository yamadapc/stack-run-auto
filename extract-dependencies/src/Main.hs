{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Set                               as Set
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.PackageDescription.Remote
import           System.Environment

extractDependencies :: String -> IO [String]
extractDependencies npkg = do
    ppkg <- getPackageLatest npkg
    case ppkg of
        ParseOk _ info ->
            case condLibrary info of
                Just lib -> return $ dependencyName <$> condTreeConstraints lib
                Nothing -> error "Package is not a library"
        _ -> error "Failed to parse package description"
  where
    dependencyName (Dependency name _) = unPackageName name

main :: IO ()
main = do
    npkgs <- getArgs
    deps <- mapConcurrently extractDependencies npkgs
    forM_ (uniq (concat deps)) putStrLn
  where
      uniq = uniq' Set.empty
        where
          uniq' _ [] = []
          uniq' x (y:ys) = if Set.member y x
                           then uniq' x ys
                           else y : uniq' (Set.insert y x) ys
