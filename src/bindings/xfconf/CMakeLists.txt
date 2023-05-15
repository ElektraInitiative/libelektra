include (FindXfconf)

if (NOT XFCONF_FOUND)
	exclude_binding (xfconf "xfconf >= 4.16 required for xfconf backend")
	return ()
endif ()

add_binding (xfconf)

set (
	XFCONF_BINDING_LIBRARY
	xfconf-elektra
	CACHE INTERNAL "XFCONF_BINDING_LIBRARY")

add_headers (ELEKTRA_HEADERS)

include_directories (${GLIB_INCLUDE_DIRS})
include_directories (${XFCONF_INCLUDE_DIRS})

file (GLOB XFCONF_BINDING_HEADERS *.h)
file (GLOB XFCONF_BINDING_SOURCES *.c)

set (SOURCES ${XFCONF_BINDING_HEADERS} ${XFCONF_BINDING_SOURCES} ${ELEKTRA_HEADERS})
add_library (${XFCONF_BINDING_LIBRARY} SHARED ${SOURCES})
target_compile_options (${XFCONF_BINDING_LIBRARY} PUBLIC)
target_link_libraries (${XFCONF_BINDING_LIBRARY} PUBLIC elektra-core elektra-kdb)

install (TARGETS ${XFCONF_BINDING_LIBRARY} LIBRARY DESTINATION lib${LIB_SUFFIX})

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
