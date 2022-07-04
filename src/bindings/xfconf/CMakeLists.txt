cmake_minimum_required(VERSION 3.22)

project (
	xfconfbinding
	VERSION 0.0.1
	LANGUAGES C)

find_package (PkgConfig QUIET)

if (NOT PKG_CONFIG_FOUND)
	exclude_binding (xfconf "Need PkgConfig needed to find dependencies for xfconf backend")
	return ()
endif ()

pkg_check_modules (XFCONF REQUIRED libxfconf-0>=4.16)

if (NOT XFCONF_FOUND)
	exclude_binding(xfconf "xfconf >= 4.16 needed for xfconf backend")
	return ()
endif()


add_binding (xfconf)

include_directories (${XFCONF_INCLUDE_DIRS})
add_library (xfconfbinding SHARED elektra-xfconf.c elektra-xfconf-channel.c elektra-xfconf-binding.c elektra-xfconf-errors.c elektra-xfconf-types.c  ${ELEKTRA_HEADERS})
target_compile_definitions (xfconfbinding PRIVATE G_ELEKTRA_SETTINGS_MODULE_PRIORITY=${GSETTINGS_MODULE_PRIORITY})

add_dependencies (xfconfbinding elektra_config_headers)
file (MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/include/elektra/)
foreach (filename ${GS_ELEKTRA_HEADERS})
	configure_file (${filename} ${CMAKE_BINARY_DIR}/include/elektra/ COPYONLY)
endforeach ()
file (GLOB GS_KDB_HEADERS ${CMAKE_SOURCE_DIR}/src/include/*.h)
foreach (filename ${GS_KDB_HEADERS})
	configure_file (${filename} ${CMAKE_BINARY_DIR}/include/ COPYONLY)
endforeach ()
include_directories (${CMAKE_BINARY_DIR}/include/)
include_directories (${CMAKE_BINARY_DIR}/src/include/)
#target_link_libraries (xfconfbinding elektra-core)
target_link_libraries (xfconfbinding INTERFACE ${XFCONF_LIBRARIES} PUBLIC elektra-core)

if (INSTALL_SYSTEM_FILES)
	install (TARGETS xfconfbinding LIBRARY DESTINATION)
endif ()

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
