{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module StackRunAuto where

import           Control.Concurrent.Async
import           Control.Lens
import           Data.Aeson.Lens
import           Data.List.Utils                 (uniq)
import           Data.Maybe                      (catMaybes)
import           Data.Monoid
import           Data.String.Utils
import qualified Data.Text                       as Text (pack, unpack)
import           Data.Time.Clock
import           Development.ExtractDependencies
import           Development.FileModules
import           Network.Wreq                    (defaults, getWith, param,
                                                  responseBody)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO                       (hPutStrLn, stderr)
import           System.Process

data Options = Options { optsFileName :: FilePath
                       , optsExtras   :: [String]
                       , optsFlags    :: [String]
                       }

run :: Options -> IO ()
run Options{..} = do
    modules <- fileModulesVerbose optsFileName
    packages <- catMaybes <$> mapConcurrently modulePackageVerbose modules
    allPackages <- mapConcurrently extractDependenciesVerbose (uniq packages)
    let argList = map ("--package " ++)
                  (filter isValidPackage
                   (uniq (packages ++ concat allPackages ++ optsExtras)))
        cmd = unwords [ "stack runghc "
                      , optsFileName
                      , unwords argList
                      , unwords optsFlags
                      ]
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

isValidPackage :: String -> Bool
isValidPackage "rts" = False
isValidPackage _ = True

fileModulesVerbose :: String -> IO [String]
fileModulesVerbose optsFileName = timed "---> Parsed imports" $ do
    putStrLn $ "Parsing " ++ optsFileName
    uniq <$> fileModulesRecur optsFileName

extractDependenciesVerbose :: String -> IO [String]
extractDependenciesVerbose pkg = timed ("---> Found dependencies for " ++ pkg) $ do
    putStrLn $ "Finding dependencies for " ++ pkg ++ "..."
    extractDependenciesCached pkg

extractDependenciesCached :: String -> IO [String]
extractDependenciesCached = cached "extract-dependencies" extractDependencies

modulePackageVerbose :: String -> IO (Maybe String)
modulePackageVerbose "" = do
    putStrLn "Skipping parse error (empty string)..."
    return Nothing
modulePackageVerbose m = timed ("---> Found package for " ++ m) $ do
    putStrLn $ "Finding package for " ++ m ++ "..."
    modulePackageCached m

cached :: (Read b, Show b) => String -> (FilePath -> IO b) -> FilePath -> IO b
cached name fn arg = do
    home <- getHomeDirectory
    let cacheDir = home </> ".stack-run-auto" </> name
        cachePth = cacheDir </> arg
    createDirectoryIfMissing True cacheDir
    exists <- doesFileExist cachePth
    if exists
        then read <$> readFile cachePth
        else do
            r <- fn arg
            writeFile cachePth (show r)
            return r

modulePackageCached :: String -> IO (Maybe String)
modulePackageCached = cached "module-package" modulePackage

modulePackage :: String -> IO (Maybe String)
modulePackage m = do
    let url = "http://hayoo.fh-wedel.de/json"
        opts = defaults & param "query" .~ ["module:" <> Text.pack m]
    res <- getWith opts url
    let result = res ^.. responseBody . key "result" . values
        moduleResults = filter isModuleResult result
    case moduleResults of
        [] -> do
            let errMsg = "No package found for " ++ m
            hPutStrLn stderr errMsg
            let mparts = split "." m
            if not (null mparts)
                then modulePackageVerbose (join "." (init mparts))
                else error $ "Couldn't resolve package for " ++ m
        (p:_) -> do
            let pkg = Text.unpack (p ^. key "resultPackage" . _String)
            return (Just pkg)
  where
    isModuleResult r = r ^. key "resultType" . _String == "module"
