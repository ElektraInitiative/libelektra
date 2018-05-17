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
                          sd  = generateSetupDeps <$> setupBuildInfo gpd
                      in concat $ catMaybes [sd, cl]
    generateSetupDeps (SetupBuildInfo d _) = d
    generateBuildDeps (CondNode _ d _) = d
    wrap x = '\'':x ++ "'"