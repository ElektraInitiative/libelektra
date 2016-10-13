include (CheckSymbolExists)
check_symbol_exists (__GNU_LIBRARY__ "features.h"    HAVE_GLIBC)

if (NOT BUILD_SHARED)
	remove_binding (intercept_open "intercept_open only works with BUILD_SHARED")
elseif (NOT HAVE_GLIBC)
	remove_binding (intercept_open "intercept_open currently needs glibc")
else ()
	include (LibAddMacros)
	add_headers (HDR_FILES)
	add_library (elektraintercept-fs SHARED "intercept.c")
	target_link_libraries (elektraintercept-fs elektra-kdb elektra-meta)
	install (TARGETS elektraintercept-fs DESTINATION lib${LIB_SUFFIX})

	create_lib_symlink (
			libelektraintercept-fs.so
			libelektraintercept.so
			)
endif ()
