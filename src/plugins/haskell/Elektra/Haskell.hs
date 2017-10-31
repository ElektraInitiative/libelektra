module Elektra.Haskell where

import Elektra.Key
import Elektra.KeySet
import Elektra.Plugin
import Foreign.Ptr

elektraHaskellOpen :: Plugin -> Key -> IO PluginStatus
elektraHaskellOpen p k = putStrLn "haskell elektraHaskellOpen" >> return Success
hs_elektraHaskellOpen = elektraPluginOpenWith elektraHaskellOpen

elektraHaskellClose :: Plugin -> Key -> IO PluginStatus
elektraHaskellClose p k = putStrLn "haskell elektraHaskellClose" >> return Success
hs_elektraHaskellClose = elektraPluginCloseWith elektraHaskellClose

elektraHaskellGet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraHaskellGet p ks k = putStrLn "haskell elektraHaskellGet" >> return NoUpdate
hs_elektraHaskellGet = elektraPluginGetWith elektraHaskellGet

elektraHaskellSet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraHaskellSet p ks k = putStrLn "haskell elektraHaskellSet" >> return NoUpdate
hs_elektraHaskellSet = elektraPluginSetWith elektraHaskellSet

elektraHaskellError :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraHaskellError p ks k = putStrLn "haskell elektraHaskellError" >> return Success
hs_elektraHaskellError = elektraPluginErrorWith elektraHaskellError

elektraHaskellCheckConfig :: Key -> KeySet -> IO PluginStatus
elektraHaskellCheckConfig k ks = putStrLn "haskell elektraHaskellCheckConfig" >> return Success
hs_elektraHaskellCheckConfig = elektraPluginCheckConfigWith elektraHaskellCheckConfig

foreign export ccall hs_elektraHaskellOpen :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellClose :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellGet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellSet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellError :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellCheckConfig :: Ptr Key -> Ptr KeySet -> IO Int
