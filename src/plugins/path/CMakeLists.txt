include (SafeCheckSymbolExists)
include (LibAddMacros)

set (plugin path)

if (DEPENDENCY_PHASE)
	add_definitions (-D_GNU_SOURCE)
	safe_check_symbol_exists (euidaccess "unistd.h" TEST_EUIDACCESS)

	if (NOT TEST_EUIDACCESS)
		remove_plugin (${plugin} "could not find euidaccess.")
	endif ()
endif ()


add_plugin (path
	    SOURCES path.h
		    path.c)
