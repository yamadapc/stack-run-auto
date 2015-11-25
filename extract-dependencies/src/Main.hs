{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                          ((&), (.~), (^.), (^..))
import           Control.Monad                         (forM_)
import           Data.Aeson.Lens                       (key, values, _Integral)
import           Data.ByteString.Lazy.Char8            (ByteString)
import qualified Data.ByteString.Lazy.Char8            as L
import           Distribution.Package                  (Dependency (..),
                                                        unPackageName)
import           Distribution.PackageDescription       (condLibrary,
                                                        condTreeConstraints)
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Network.Wreq
import           System.Environment                    (getArgs)

getLatestRevision :: String -> IO Integer
getLatestRevision package =
  maximum . (^.. responseBody . values . key "number" . _Integral) <$> getWith opt uri
  where opt = defaults & header "Accept" .~ ["application/json"]
        uri = "http://hackage.haskell.org/package/" ++ package ++ "/revisions/"

getPackageRevision :: String -> Integer -> IO ByteString
getPackageRevision package revision =
  (^. responseBody) <$> get uri
  where uri = "http://hackage.haskell.org/package/" ++ package ++ "/revision/" ++ show revision

parseCabal :: ByteString -> Either String [String]
parseCabal file =
  case parsePackageDescription (L.unpack file) of
      ParseOk _ info -> case condLibrary info of
                            Just lib -> Right $ dependencyName <$> condTreeConstraints lib
                            Nothing -> Left "Package is not a library."
      ParseFailed e -> Left $ show e
  where dependencyName (Dependency name _) = unPackageName name

main :: IO ()
main = do
  package <- head <$> getArgs
  getLatestRevision package >>= getPackageRevision package >>= display . parseCabal
  where display ds = case ds of
                         Left e -> error e
                         Right ds' -> forM_ ds' putStrLn
