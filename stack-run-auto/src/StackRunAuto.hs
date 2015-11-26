{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module StackRunAuto where

import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad                   (forM_, void)
import           Data.Aeson.Lens
import           Data.List.Utils                 (uniq)
import           Data.Monoid
import           Data.String.Utils
import qualified Data.Text                       as Text (pack, unpack)
import           Data.Time.Clock
import           Development.ExtractDependencies
import           Development.FileModules
import           Network.Wreq                    (defaults, getWith, param,
                                                  responseBody)
import qualified STMContainers.Set               as Set
import           System.Exit
import           System.Process

data Options = Options { optsFileName :: FilePath
                       }

run :: Options -> IO ()
run Options{..} = do
    modules <- fileModulesVerbose optsFileName
    packages <- mapConcurrently modulePackageVerbose modules
    allPackages <- mapConcurrently extractDependenciesVerbose (uniq packages)
    let argList = map ("--package " ++) (uniq (packages ++ concat allPackages))
        cmd = "stack runghc " ++ optsFileName ++ " " ++ join " " argList
    putStrLn cmd
    ph <- runCommand cmd
    waitForProcess ph >>= exitWith

timed :: String -> IO a -> IO a
timed msg action = do
    start <- getCurrentTime
    ret <- action
    end <- getCurrentTime
    putStrLn $ msg ++ " (" ++ show (diffUTCTime end start) ++ ")"
    return ret

fileModulesVerbose :: String -> IO [String]
fileModulesVerbose optsFileName = timed "---> Parsed imports" $ do
    putStrLn $ "Parsing " ++ optsFileName
    filesSeen <- Set.newIO
    uniq <$> fileModulesRecur filesSeen optsFileName

extractDependenciesVerbose :: String -> IO [String]
extractDependenciesVerbose pkg = timed ("---> Found dependencies for " ++ pkg) $ do
    putStrLn $ "Finding dependencies for " ++ pkg ++ "..."
    extractDependencies pkg

modulePackageVerbose :: String -> IO String
modulePackageVerbose m = timed ("---> Found package for " ++ m) $ do
    putStrLn $ "Finding package for " ++ m ++ "..."
    modulePackage m

modulePackage :: String -> IO String
modulePackage m = do
    let url = "http://hayoo.fh-wedel.de/json"
        opts = defaults & param "query" .~ ["module:" <> Text.pack m]
    res <- getWith opts url
    let result = res ^.. responseBody . key "result" . values
        moduleResults = filter isModuleResult result
    case moduleResults of
        [] -> error $ "No package found for " ++ m
        (p:_) -> do
            let pkg = Text.unpack (p ^. key "resultPackage" . _String)
            return pkg
  where
    isModuleResult r = r ^. key "resultType" . _String == "module"
