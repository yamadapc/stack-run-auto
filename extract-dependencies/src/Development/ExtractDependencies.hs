module Development.ExtractDependencies where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.PackageDescription.Remote

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
