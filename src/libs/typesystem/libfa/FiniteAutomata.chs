--
-- @file
--
-- @brief LibFA Haskell Bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module FiniteAutomata (FiniteAutomata, State, BasicAutomata (..),
  compile, makeBasic, asRegexp, minimize, FiniteAutomata.concat, union, 
  intersect, complement, minus, iter, contains, equals, overlap) where

#include <fa.h>
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (Ptr, castPtr, nullPtr)
import Foreign.ForeignPtr    (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Storable      (peek)
import Foreign.C.Types       (CChar)
import Foreign.C.String      (withCString)

{#context lib="libfa" prefix = "fa" #}

{#pointer *fa    as FiniteAutomata foreign finalizer free newtype #}
{#pointer *state as State          foreign                newtype #}

{#enum fa_basic as BasicAutomata { underscoreToCase } deriving (Show, Eq) #}

{#typedef size_t  Int #}
{#typedef ssize_t Int #}

--
-- Convert from/to Regex
--
{#fun unsafe make_basic as makeBasic {`BasicAutomata'} -> `FiniteAutomata' #}

-- Implemented manually to get around that double Ptr issue with c2hs
compile :: String -> IO (Either Int FiniteAutomata)
compile str = withCString str $ \a1' -> 
  alloca $ \a3' -> 
  fa_compile a1' (length str) a3' >>= \res ->
  let {res' = fromIntegral res} in
  peek a3' >>= \a3'' ->
  newForeignPtr fa_free (castPtr a3'') >>= \a3''' ->
  withForeignPtr a3''' $ \a3'''' -> 
  if a3'''' == nullPtr then return $ Left res' else return $ Right $ FiniteAutomata a3'''

{#fun unsafe as_regexp as asRegexp {`FiniteAutomata', alloca- `Ptr CChar' peek*, alloca- `Int' peek*} -> `Int' #}

--
-- Finite Automata Algorithms
--
{#fun unsafe minimize   {`FiniteAutomata'}                   -> `Int' #}
{#fun unsafe concat     {`FiniteAutomata', `FiniteAutomata'} -> `FiniteAutomata' #}
{#fun unsafe union      {`FiniteAutomata', `FiniteAutomata'} -> `FiniteAutomata' #}
{#fun unsafe intersect  {`FiniteAutomata', `FiniteAutomata'} -> `FiniteAutomata' #}
{#fun unsafe complement {`FiniteAutomata'}                   -> `FiniteAutomata' #}
{#fun unsafe minus      {`FiniteAutomata', `FiniteAutomata'} -> `FiniteAutomata' #}
{#fun unsafe iter       {`FiniteAutomata', `Int', `Int'}     -> `FiniteAutomata' #}
{#fun unsafe contains   {`FiniteAutomata', `FiniteAutomata'} -> `Int' #}
{#fun unsafe equals     {`FiniteAutomata', `FiniteAutomata'} -> `Int' #}
{#fun unsafe overlap    {`FiniteAutomata', `FiniteAutomata'} -> `FiniteAutomata' #}

foreign import ccall unsafe "FiniteAutomata.chs.h fa_compile"
  fa_compile :: ((C2HSImp.Ptr C2HSImp.CChar) -> (Int -> ((C2HSImp.Ptr (C2HSImp.Ptr ())) -> (IO C2HSImp.CInt))))
