include (LibAddHaskellPlugin)

find_package (LibFA QUIET)

if (LIBFA_FOUND)
	add_haskell_plugin (regexdispatcher
			    MODULE
			    "RegexDispatcher"
			    SANDBOX_ADD_SOURCES
			    "src/libs/typesystem/libfa")
else (LIBFA_FOUND)
	remove_plugin (regexdispatcher "libfa not found (usually bundled with augeas), but required for the regexdispatcher")
endif (LIBFA_FOUND)
