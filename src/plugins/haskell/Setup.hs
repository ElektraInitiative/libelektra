--
-- Collects information about the libraries used to build this plugin
-- so they can be statically linked afterwards in order to provide
-- easy-to-package haskell plugins for elektra 
--
{-# LANGUAGE CPP #-}

module Main (main) where

import Distribution.Simple
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Control.Monad        (forM_, when)
import Control.Applicative  (liftA2)
import Data.Foldable        (toList)
import Data.Maybe           (fromJust, listToMaybe, mapMaybe)
import Data.Set             (Set)
import Data.List            (isSuffixOf, isInfixOf)
import System.IO            (hPutStrLn, stderr)
import System.Exit          (exitFailure)
import System.Process       (readProcess)
import System.Directory     (createDirectoryIfMissing)

import qualified Data.Set                              as S
import qualified Distribution.Simple.Compiler          as C
import qualified Distribution.Simple.Setup             as C
import qualified Distribution.Types.LocalBuildInfo     as C
import qualified Distribution.Types.PackageDescription as C
import qualified Distribution.Package                  as C
import qualified Distribution.Simple.LocalBuildInfo    as C
import qualified Distribution.Simple.PackageIndex      as C
import qualified Distribution.Text                     as C
import qualified Distribution.Simple.Program           as C
import qualified Distribution.Verbosity                as V
import qualified Distribution.InstalledPackageInfo     as IPI

#if MIN_VERSION_Cabal(1,24,0)
type PackageIndex a = C.PackageIndex IPI.InstalledPackageInfo
#elif MIN_VERSION_Cabal(1,22,0)
type PackageIndex a = C.PackageIndex (IPI.InstalledPackageInfo_ a)
#else
type PackageIndex a = C.PackageIndex
#endif
data Build = Static | Dynamic

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { hookedPrograms = [C.simpleProgram "libtool", C.simpleProgram "ln"],
                                               postBuild = elektraHaskellPluginPostBuildHook})

elektraHaskellPluginPostBuildHook :: Args -> C.BuildFlags -> C.PackageDescription -> C.LocalBuildInfo -> IO ()
elektraHaskellPluginPostBuildHook _ flags pd lbi = do
  logV "Preparing the Haskell dependencies.."
  isVerbose $ forM_ libraries putStrLn
  let ln = C.lookupProgram (C.simpleProgram "ln") $ C.withPrograms lbi
  let maybeMainLink = fmap mainLink ln
  let maybeMainDepLink = fmap mainDepLink ln
  let maybeDepLink = fmap depLink ln
  let maybeDepLinks = fmap (`fmap` dependencies) maybeDepLink
  let maybeLink = liftA2 (:) (Just $ createDirectoryIfMissing True "haskell")
                $ liftA2 (:) maybeMainDepLink
                $ liftA2 (:) maybeMainLink maybeDepLinks
  maybe linkFailure (\l -> sequence_ l >> logV "Finished generating the post-build output..") maybeLink
  -- let maybeLibtool = fmap packWithLibtool $ C.lookupProgram (C.simpleProgram "libtool") $ C.withPrograms lbi
  -- let maybeAr = fmap packWithAr $ C.lookupProgram (C.simpleProgram "ar") $ C.withPrograms lbi
  -- maybe packFailure (>> logV "Finished packing all the haskell dependencies..") (maybeLibtool <|> maybeAr)
  where
    verbosity = C.fromFlagOrDefault V.normal $ C.buildVerbosity flags
    isVerbose = when (verbosity >= V.verbose)
    logV = isVerbose . putStrLn
    failure str = hPutStrLn stderr str >> exitFailure
    packFailure = failure "Failed to locate libtool and ar, no program to pack the dependencies"
    linkFailure = failure "Failed to locate ln"
    libraryName = "lib" ++ C.getHSLibraryName (C.localUnitId lbi) ++ suffix lbi
    libraryPath = C.buildDir lbi ++ "/" ++ libraryName
    libraries = (libraryPath :) $ map (showLibrary lbi) dependencies
    dependencies = filter (not . isGlobalLibrary) $ getDependencyInstalledPackageInfos lbi
    createMRIScript sl = ("create " ++ libraryName) : fmap ("addlib " ++) sl ++ ["save", "end"]
    packWithAr ar = logV "packing with ar" >>
      readProcess (C.programPath ar) ["-M"] (unlines $ createMRIScript libraries) >>= logV
    packWithLibtool lt = logV "packing with libtool" >>
      readProcess (C.programPath lt) ("-static" : "-o" : libraryName : libraries) "" >>= logV
    mainLink ln = let filename = "libHS" ++ (C.unPackageName . C.packageName . C.package) pd ++ suffix lbi in 
      logV "linking main library" >>
      readProcess (C.programPath ln) ["-f", libraryPath, filename] "" >>= logV
    mainDepLink ln = logV "linking main library for libraries" >>
      readProcess (C.programPath ln) ["-f", libraryPath, "haskell/" ++ libraryName] "" >>= logV
    depLink ln lib = logV ("linking library " ++ showLibrary lbi lib) >>
      readProcess (C.programPath ln) ["-f", showLibrary lbi lib, "haskell/libHS" ++ getLibraryName lib ++ suffix lbi] ""
      >>= logV

-- The globally installed ghc libs which we don't want to link statically
-- We only want to link external dependencies grabbed via hackage, not those
-- that will be present on target systems due to the ghc requirement.
-- Later on we can include a flag to optionally link everything statically
-- thus target platforms wouldn't require ghc to be installed.
ghcLibs :: [String]
ghcLibs = ["base", "ghc-prim", "integer-gmp", "rts"]

isGlobalLibrary :: InstalledPackageInfo -> Bool
isGlobalLibrary = liftA2 (||) blacklisted global
  where
    blacklisted = flip any ghcLibs . (==) . C.display . C.pkgName . IPI.sourcePackageId
    global      = not . any (".cabal-sandbox" `isInfixOf`) . IPI.libraryDirs

findTransitiveDependencies :: PackageIndex a -> Set C.UnitId -> Set C.UnitId
findTransitiveDependencies pkgIdx initial = go S.empty (S.toList initial)
  where
    go set []  = set
    go set (q : qs)
      | q `S.member` set = go set qs
      | otherwise        = maybe (go set qs) (go (S.insert q set) . (++ qs) . IPI.depends)
                         $ C.lookupUnitId pkgIdx q

getDependencyInstalledPackageIds :: C.LocalBuildInfo -> Set C.UnitId
getDependencyInstalledPackageIds lbi = findTransitiveDependencies (C.installedPkgs lbi) $
  S.fromList [ ipId | componentLbi <- toList (C.componentGraph lbi)
                    , (ipId, _)    <- C.componentPackageDeps componentLbi]

getDependencyInstalledPackageInfos :: C.LocalBuildInfo -> [InstalledPackageInfo]
getDependencyInstalledPackageInfos lbi = mapMaybe (C.lookupUnitId $ C.installedPkgs lbi)
                                       $ S.toList (getDependencyInstalledPackageIds lbi)

showLibrary :: C.LocalBuildInfo -> InstalledPackageInfo -> String
showLibrary lbi ipi = fromJust (getLibraryDir lbi ipi) ++ "/libHS" ++ getLibraryName ipi ++ suffix lbi

getLibraryDir :: C.LocalBuildInfo -> InstalledPackageInfo -> Maybe String
getLibraryDir lbi ipi = listToMaybe $ case buildType lbi of
  Static  -> filter (getLibraryName ipi `isSuffixOf`) $ IPI.libraryDirs ipi
  Dynamic -> IPI.libraryDynDirs ipi

getLibraryName :: InstalledPackageInfo -> String
getLibraryName = C.display . IPI.installedUnitId

buildType :: C.LocalBuildInfo -> Build
buildType lbi = if C.withSharedLib lbi then Dynamic else Static

suffix ::  C.LocalBuildInfo -> String
suffix lbi = case buildType lbi of
  Static  -> ".a"
  Dynamic -> '-' : filter (/= '-') (C.showCompilerId $ C.compiler lbi) ++ ".dylib"
