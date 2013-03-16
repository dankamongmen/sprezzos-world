
module Main (main) where

import Data.Version ( showVersion )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version

main :: IO ()
main = do fp <- findPackageDesc "."
          pd <- readPackageDescription normal fp
          putStr $ showVersion $ pkgVersion $ package $ packageDescription pd

