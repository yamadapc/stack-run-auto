{-# LANGUAGE OverloadedStrings #-}
module Distribution.PackageDescription.Remote
  where

import           Control.Lens                          ((&), (.~), (^.), (^..))
import           Data.Aeson.Lens                       (key, values, _Integral)
import           Data.ByteString.Lazy.Char8            (unpack)
import           Distribution.PackageDescription       (GenericPackageDescription)
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Network.Wreq                          (defaults, get, getWith,
                                                        header, responseBody)

getPackage :: String -> Integer -> IO (ParseResult GenericPackageDescription)
getPackage pkg revision = do
    res <- get uri
    return $ parsePackageDescription (unpack (res ^. responseBody))
  where
    uri = "http://hackage.haskell.org/package/" ++ pkg ++ "/revision/" ++
          show revision

getPackageRevisions :: String -> IO [Integer]
getPackageRevisions pkg = do
    res <- getWith opt uri
    return $ res ^.. responseBody . values . key "number" . _Integral
  where
    opt = defaults & header "Accept" .~ ["application/json"]
    uri = "http://hackage.haskell.org/package/" ++ pkg ++ "/revisions/"

getPackageLatestRevision :: String -> IO Integer
getPackageLatestRevision pkg = do
    revisions <- getPackageRevisions pkg
    return (maximum revisions)
