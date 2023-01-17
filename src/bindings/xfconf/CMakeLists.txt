cmake_minimum_required (VERSION 3.22)

project (
	xfconfbinding
	VERSION 0.0.1
	LANGUAGES C)

include (FindXfconf)

if (NOT XFCONF_FOUND)
	exclude_binding (xfconf "xfconf >= 4.16 required for xfconf backend")
	return ()
endif ()

add_binding (xfconf)

add_headers (ELEKTRA_HEADERS)

include_directories (${GLIB_INCLUDE_DIRS})
include_directories (${XFCONF_INCLUDE_DIRS})
add_library (xfconfbinding SHARED elektra-xfconf.c elektra-xfconf-channel.c elektra-xfconf-binding.c elektra-xfconf-errors.c
				  elektra-xfconf-types.c ${ELEKTRA_HEADERS})
target_compile_definitions (xfconfbinding PRIVATE)
target_link_libraries (xfconfbinding PUBLIC elektra-core elektra-kdb)

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	if (NOT FORCE_IN_SOURCE_BUILD)
		message (
			FATAL_ERROR
				"In-source builds are not permitted.\n"
				"Make a separate folder for building:\n"
				"    mkdir build && cd build && cmake ..\n"
				"Before that, remove the files already created:\n"
				"    rm -rf CMakeCache.txt CMakeFiles\n"
				"If you really know what you are doing\n"
				"(will overwrite original files!) use:\n"
				"    cmake -DFORCE_IN_SOURCE_BUILD=ON\n")
	endif (NOT FORCE_IN_SOURCE_BUILD)
endif (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
