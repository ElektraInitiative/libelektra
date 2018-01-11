{-# LANGUAGE TypeFamilies, UndecidableInstances,
  ExistentialQuantification, TypeInType, GADTs, TypeOperators #-}
module Elektra.Typechecker where

import Elektra.Key
import Elektra.KeySet
import Elektra.Plugin
import Elektra.SpecTranslator
import Elektra.SpecParser

import Control.Monad                (mapM_)
import Text.PrettyPrint
import Language.Haskell.Exts.Pretty
import Language.Haskell.Interpreter
import Data.List                    (intercalate, isPrefixOf)
import Control.Monad                (mapM_)
import System.IO.Temp               (writeTempFile)
import System.Directory             (removeFile)

import Foreign.Ptr

typecheck :: KeySet -> Key -> IO ()
typecheck ks k = do
  putStepLn "Parse Specifications now."

  typeSpecs <- parseTypeSpecifications ks
  putStrLn $ "Got " ++ show (length typeSpecs) ++ " function specifications"
  mapM_ print typeSpecs
  putEmptyLn

  keySpecs <- parseKeySpecifications ks
  putStrLn $ "Got " ++ show (length keySpecs) ++ " key specifications"
  mapM_ print keySpecs

  putStepLn "Done Parsing all Specifications, now translate them."
  let rendered = renderStyle style {lineLength = 320} $ prettyPrim $ translateSpecifications typeSpecs keySpecs
  putStrLn rendered
  
  putStepLn "Done Translating all Specifications."
  specFile <- writeTempFile "/tmp/" "testSpecification.hs" rendered
  r <- runInterpreter (keyInterpreter specFile)
  removeFile specFile
  case r of
    Left err -> do
      -- TODO use error code
      _ <- keySetMeta k "specElektra/typeError" (errorString err)
      putStrLn $ errorString err
    Right () -> return ()
  where
    putStepLn s = putEmptyLn >> putStrLn s >> putEmptyLn
    putEmptyLn = putStrLn ""

keyInterpreter :: FilePath -> InterpreterT IO ()
keyInterpreter specFile = do
  -- this way we don't need to have it installed in the system
  -- TODO make this more portable or configurable
  loadModules ["/Users/admin/git/libelektra/src/libs/typesystem/SpecElektra/Elektra/specElektra.hs", specFile]
  say "Loaded our rendered specification, now we can infer the stuff in it."
  setTopLevelModules ["TestSpecification"]
  getModuleExports "TestSpecification" >>= mapM_ showTypes . filter isFun . filter (not . isPrefixOf "specElektraRawKey" . name)
  where
    showTypes c = typeOf (name c) >>= say . ((name c ++ " has an infered type of ") ++)
    isFun (Fun _) = True
    isFun _ = False

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

elektraTypecheckerOpen :: Plugin -> Key -> IO PluginStatus
elektraTypecheckerOpen _ _ = return Success
hs_elektraHaskellOpen = elektraPluginOpenWith elektraTypecheckerOpen

elektraTypecheckerClose :: Plugin -> Key -> IO PluginStatus
elektraTypecheckerClose _ _ = return Success
hs_elektraHaskellClose = elektraPluginCloseWith elektraTypecheckerClose

elektraTypecheckerGet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraTypecheckerGet _ ks k = typecheck ks k >> return NoUpdate
hs_elektraHaskellGet = elektraPluginGetWith elektraTypecheckerGet

elektraTypecheckerSet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraTypecheckerSet _ ks k = typecheck ks k >> return NoUpdate
hs_elektraHaskellSet = elektraPluginSetWith elektraTypecheckerSet

elektraTypecheckerError :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraTypecheckerError _ _ _ = return Success
hs_elektraHaskellError = elektraPluginErrorWith elektraTypecheckerError

foreign export ccall hs_elektraHaskellOpen :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellClose :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellGet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellSet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellError :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
