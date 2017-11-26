module Elektra.Plugin (Plugin, elektraPluginGetConfig, elektraPluginSetData, elektraPluginGetData,
    elektraPluginOpenWith, elektraPluginCloseWith, elektraPluginGetWith, elektraPluginSetWith,
    elektraPluginErrorWith, elektraPluginCheckConfigWith,
    PluginStatus (..)) where

{#import Elektra.Key#}
{#import Elektra.KeySet#}
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (newForeignPtr_)
import Control.Monad (join, liftM, liftM2, liftM3)

#include <kdbplugin.h>

{#pointer *Plugin foreign newtype #}

-- ***
-- CONSTANTS
-- **

data PluginStatus = Error | NoUpdate | Success deriving (Show, Eq)
instance Enum PluginStatus where
    fromEnum Error = -1
    fromEnum NoUpdate = 0
    fromEnum Success = 1

    toEnum (-1) = Error
    toEnum 0 = NoUpdate
    toEnum 1 = Success
    toEnum unmatched = error ("PluginStatus.toEnum: Cannot match " ++ show unmatched)

-- ***
-- PLUGIN METHODS
-- ***

{#fun unsafe elektraPluginGetConfig {`Plugin'} -> `KeySet' #}
-- You have to cast it using Foreign.Ptr.castPtr to the data structure you use manually
{#fun unsafe elektraPluginSetData {`Plugin', `Ptr ()'} -> `()' #}
{#fun unsafe elektraPluginGetData {`Plugin'} -> `Ptr ()' #}

-- ***
-- PLUGIN STUB METHODS
-- ***

elektraPluginOpenWith :: (Plugin -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr Key -> IO Int
elektraPluginOpenWith = elektraPlugin2

elektraPluginCloseWith :: (Plugin -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr Key -> IO Int
elektraPluginCloseWith = elektraPlugin2

elektraPluginGetWith :: (Plugin -> KeySet -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
elektraPluginGetWith = elektraPlugin3

elektraPluginSetWith :: (Plugin -> KeySet -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
elektraPluginSetWith = elektraPlugin3

elektraPluginErrorWith :: (Plugin -> KeySet -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
elektraPluginErrorWith = elektraPlugin3

elektraPluginCheckConfigWith :: (Key -> KeySet -> IO PluginStatus) -> Ptr Key -> Ptr KeySet -> IO Int
elektraPluginCheckConfigWith f k ks = liftM fromEnum $ join $ liftM2 f (liftM Key $ newForeignPtr_ k) (liftM KeySet $ newForeignPtr_ ks)

-- shared parameter conversation

elektraPlugin2 :: (Plugin -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr Key -> IO Int
elektraPlugin2 f p k = liftM fromEnum $ join $ liftM2 f (liftM Plugin $ newForeignPtr_ p) (liftM Key $ newForeignPtr_ k)

elektraPlugin3 :: (Plugin -> KeySet -> Key -> IO PluginStatus) -> Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
elektraPlugin3 f p ks k = liftM fromEnum $ join $ liftM3 f (liftM Plugin $ newForeignPtr_ p) (liftM KeySet $ newForeignPtr_ ks) (liftM Key $ newForeignPtr_ k)
