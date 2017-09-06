module Elektra.Haskelltemplate where

import Elektra.Key
import Elektra.KeySet
import Elektra.Plugin
import Foreign.Ptr

elektraHaskelltemplateOpen :: Plugin -> Key -> IO PluginStatus
elektraHaskelltemplateOpen p k = putStrLn "haskell elektraHaskelltemplateOpen" >> return Success
hs_elektraHaskelltemplateOpen = elektraPluginOpenWith elektraHaskelltemplateOpen

elektraHaskelltemplateClose :: Plugin -> Key -> IO PluginStatus
elektraHaskelltemplateClose p k = putStrLn "haskell elektraHaskelltemplateClose" >> return Success
hs_elektraHaskelltemplateClose = elektraPluginCloseWith elektraHaskelltemplateClose

elektraHaskelltemplateGet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraHaskelltemplateGet p ks k = putStrLn "haskell elektraHaskelltemplateGet" >> return NoUpdate
hs_elektraHaskelltemplateGet = elektraPluginGetWith elektraHaskelltemplateGet

elektraHaskelltemplateSet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraHaskelltemplateSet p ks k = putStrLn "haskell elektraHaskelltemplateSet" >> return NoUpdate
hs_elektraHaskelltemplateSet = elektraPluginSetWith elektraHaskelltemplateSet

elektraHaskelltemplateError :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraHaskelltemplateError p ks k = putStrLn "haskell elektraHaskelltemplateError" >> return Success
hs_elektraHaskelltemplateError = elektraPluginErrorWith elektraHaskelltemplateError

elektraHaskelltemplateCheckConfig :: Key -> KeySet -> IO PluginStatus
elektraHaskelltemplateCheckConfig k ks = putStrLn "haskell elektraHaskelltemplateCheckConfig" >> return Success
hs_elektraHaskelltemplateCheckConfig = elektraPluginCheckConfigWith elektraHaskelltemplateCheckConfig

foreign export ccall hs_elektraHaskelltemplateOpen :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskelltemplateClose :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskelltemplateGet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskelltemplateSet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskelltemplateError :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskelltemplateCheckConfig :: Ptr Key -> Ptr KeySet -> IO Int