--
-- @file
--
-- @brief Reads the given cabal files and generates a cabal install
-- command including each one of them. Used together with the script
-- ./generate_haskell_dependencies which scans for all cabal files.
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Text
import Distribution.Types.Dependency
import Distribution.Types.PackageId
import Distribution.Types.PackageName

import Data.List (nub, intercalate)
import Data.Maybe (catMaybes)
import Data.Char (isSpace)

import qualified Data.ByteString as BS

generateHaskellDependencies :: [String] -> IO ()
generateHaskellDependencies cabalFiles = do
  deps <- mapM (readGenericPackageDescription normal) cabalFiles
  let exclusions = (mkPackageName "Cabal") : map (pkgName . package . packageDescription) deps
  let uniqueDeps = nub . concat . map generateDeps $ deps
  let filteredDeps = filter (flip notElem exclusions . depPkgName) uniqueDeps
  putStr "cabal install "
  putStrLn . intercalate " " . map (wrap . show . disp) $ filteredDeps
  where
    generateDeps pd = let gpd = packageDescription pd
                          cl  = generateBuildDeps <$> condLibrary pd
                          csl = gen condSubLibraries pd
                          cfl = gen condForeignLibs pd
                          ce  = gen condExecutables pd
                          cts = gen condTestSuites pd
                          cb  = gen condBenchmarks pd
                          sd  = generateSetupDeps <$> setupBuildInfo gpd
                      in concat . catMaybes $ [sd, cl, csl, cfl, ce, cts, cb, sd]
                      where gen f = Just . concatMap (generateBuildDeps . snd) . f
    generateSetupDeps (SetupBuildInfo d _) = d
    generateBuildDeps (CondNode _ d _) = d
    wrap x = '\'':x ++ "'"
