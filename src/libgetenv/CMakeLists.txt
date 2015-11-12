include(CheckSymbolExists)
check_symbol_exists(__GNU_LIBRARY__ "features.h"    HAVE_GLIBC)

if (BUILD_SHARED AND HAVE_GLIBC)
	add_subdirectory(include)
	include_directories(include)

	set(NAME elektrify-getenv)

	generate_readme(${NAME})
	generate_manpage(kdb-${NAME} FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
	include_directories(${CMAKE_CURRENT_BINARY_DIR})

	add_subdirectory(src)
	add_subdirectory(examples)
	add_subdirectory(benchmarks)

	if (ENABLE_TESTING)
		if (BUILD_FULL OR BUILD_STATIC)
			add_subdirectory(tests)
		endif (BUILD_FULL OR BUILD_STATIC)
	endif(ENABLE_TESTING)
endif ()
