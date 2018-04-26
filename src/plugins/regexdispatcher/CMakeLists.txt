include (LibAddHaskellPlugin)

find_package (fa)

if (LIBFA_FOUND)
	
	add_haskell_plugin (regexdispatcher
		SANDBOX_ADD_SOURCES
		"src/bindings/haskell"
		"src/libs/typesystem/libfa"
	)

else (LIBFA_FOUND)
	remove_plugin (regexdispatcher "libfa not found (usually bundled with augeas), but required for haskell plugins")
endif (LIBFA_FOUND)
