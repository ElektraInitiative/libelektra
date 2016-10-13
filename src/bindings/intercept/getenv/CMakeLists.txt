include (CheckSymbolExists)
check_symbol_exists (__GNU_LIBRARY__ "features.h"    HAVE_GLIBC)

if (NOT BUILD_SHARED)
	remove_binding (intercept_getenv "intercept_getenv only works with BUILD_SHARED")
elseif (NOT HAVE_GLIBC)
	remove_binding (intercept_getenv "intercept_getenv currently needs glibc")
else ()
	add_subdirectory (include)
	include_directories (include)

	set (NAME elektrify-getenv)

	generate_readme (${NAME})
	generate_manpage (kdb-${NAME} FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	add_subdirectory (src)
	add_subdirectory (examples)
	add_subdirectory (benchmarks)

	if (ENABLE_TESTING)
		add_subdirectory (tests)
	endif (ENABLE_TESTING)
endif ()
