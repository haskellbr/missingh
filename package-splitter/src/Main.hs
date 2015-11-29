{-# LANGUAGE RecordWildCards #-}
module Main where

-- import           Options.Applicative
import           Control.Monad                         (filterM, forM)
import           Data.List                             (find, isSuffixOf)
import           Data.String.Utils                     (join)
import           Development.FileModules
import           Distribution.ModuleName               (ModuleName, fromString)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf

main :: IO ()
main = do
    args <- getArgs
    case args of
        (dirName:_) -> do
            pkg <- packageAtDirectory dirName
            splitPackage pkg
        _ -> error "Invalid arguments"

data Package = Package { packagePath               :: FilePath
                       , packageGenericDescription :: GenericPackageDescription
                       }

data Module = Module { moduleFPath :: FilePath
                     , moduleName  :: ModuleName
                     }
  deriving(Show)

splitPackage :: Package -> IO ()
splitPackage pkg = packageModulesIO pkg >>= mapM_ (splitModule pkg)

splitModule :: Package -> Module -> IO ()
splitModule _pkg m = do
    ms <- fileModulesRecur (moduleFPath m)
    print ms

-- TODO refactor this shit and getDirectoryModules
packageModulesIO :: Package -> IO [Module]
packageModulesIO Package{..} = packageLibModules
  where
    clib = condTreeData <$> condLibrary packageGenericDescription
    packageLibModules | Just lib <- clib =
        concat <$> mapM (getDirectoryModules . (takeDirectory packagePath </>))
                        (hsSourceDirs (libBuildInfo lib))
                      | otherwise = return []

-- |
-- Recursivelly lists modules under a directory
getDirectoryModules :: FilePath -> IO [Module]
getDirectoryModules fp = print fp >> go [] [] fp
  where
    isHaskellFile = (== ".hs") . takeExtension
    go modPrefix mods dir = do
        potentialModules <- map (dir </>) <$>
                            filter (\f -> f /= "." && f /= "..") <$>
                            getDirectoryContents dir
        dirs <- filterM doesDirectoryExist potentialModules

        let modFs = filter isHaskellFile potentialModules
            mods' = mods ++ map moduleAtFile modFs
        case dirs of
            [] -> return mods'
            ds -> concat <$> forM ds (\d ->
                go (modPrefix ++ [takeBaseName d]) mods' d)
      where
        moduleAtFile mf = Module { moduleFPath = mf
                                 , moduleName = fromString $
                                       join "." (modPrefix ++ [takeBaseName mf])
                                 }

packageAtDirectory :: FilePath -> IO Package
packageAtDirectory fp = do
    printf "Looking for .cabal file in %s\n" fp
    fs <- getDirectoryContents fp
    case find (".cabal" `isSuffixOf`) fs of
         Just cabalFile -> do
             let pkgPath = fp </> cabalFile
             printf "Using %s\n" pkgPath
             result <- parsePackageDescription <$> readFile pkgPath
             case result of
                 ParseOk _ pkgDesc -> return $ Package pkgPath pkgDesc
                 _ -> error (printf "Failed to parse %s" pkgPath)

         Nothing -> error (printf "Couldn't find a cabal file in %s" fp)
