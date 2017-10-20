include (LibAddHaskellPlugin)

if (GHC_FFI_LIB)
if (GHC_RTS_LIB)

add_haskell_plugin (haskell)

else (GHC_RTS_LIB)
	remove_plugin (haskell "GHC_RTS_LIB not found")
endif (GHC_RTS_LIB)
else (GHC_FFI_LIB)
	remove_plugin (haskell "GHC_FFI_LIB not found")
endif (GHC_FFI_LIB)
