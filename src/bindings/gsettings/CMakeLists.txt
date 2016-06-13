cmake_minimum_required (VERSION 2.8)
project (elektrasettings)

find_package (PkgConfig REQUIRED)
pkg_check_modules (GLIB glib-2.0>=2.42 QUIET)
pkg_check_modules (GMODULE gmodule-2.0>=2.42 QUIET)
pkg_check_modules (GIO gio-2.0>=2.42 QUIET)
if (NOT GLIB_FOUND)
	remove_binding (gsettings "glib >= 2.42 needed for gsettings backend")
	return ()
endif ()
if (NOT GMODULE_FOUND)
	remove_binding (gsettings "gmodule >= 2.42 needed for gsettings backend")
	return ()
endif ()
if (NOT GIO_FOUND)
	remove_binding (gsettings "gio >= 2.42 needed for gsettings backend")
	return ()
endif ()

include_directories (${GLIB_INCLUDE_DIRS})
add_library (elektrasettings SHARED elektrasettingsbackend.c ${ELEKTRA_HEADERS})
add_dependencies (elektrasettings elektra_config_headers)
set (GSETTINGS_MODULE_PRIORITY 200 CACHE STRING "GIO Module Priority. Lower then 100 means dconf backend will be default.")
target_compile_definitions (elektrasettings PRIVATE G_ELEKTRA_SETTINGS_MODULE_PRIORITY=${GSETTINGS_MODULE_PRIORITY})
if (GELEKTRA_LIBRARY)
	file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/include/elektra/)
	file (GLOB GS_ELEKTRA_HEADERS ../glib/*.h)
	foreach (filename ${GS_ELEKTRA_HEADERS})
		configure_file (${filename} ${CMAKE_BINARY_DIR}/include/elektra/ COPYONLY)
	endforeach ()
	file (GLOB GS_KDB_HEADERS ${CMAKE_SOURCE_DIR}/src/include/*.h)
	foreach (filename ${GS_KDB_HEADERS})
		configure_file (${filename} ${CMAKE_BINARY_DIR}/include/ COPYONLY)
	endforeach ()
	include_directories (${CMAKE_BINARY_DIR}/include/)
	include_directories (${CMAKE_BINARY_DIR}/src/include/)
	target_link_libraries (elektrasettings ${GLIB_LIBRARIES} ${GMODULE_LIBRARIES} ${GIO_LIBRARIES} ${GELEKTRA_LIBRARY} elektra-core)
else()
	pkg_check_modules (GELEKTRA gelektra-4.0>=0.8.16 QUIET)
	if (!GELEKTRA_FOUND)
		remove_binding (glib "elektra glib bindings needed for gsettings backend")
	endif()
	include_directories (${GELEKTRA_INCLUDE_DIRS})
	target_link_libraries (elektrasettings ${GLIB_LIBRARIES} ${GMODULE_LIBRARIES} ${GIO_LIBRARIES} ${GELEKTRA_LIBRARIES})
endif ()
if (CMAKE_VERSION GREATER 3.3)
	pkg_get_variable (GIO_MODULE_DIR gio-2.0 giomoduledir)
else ()
	execute_process (
		COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=giomoduledir gio-2.0
		OUTPUT_VARIABLE GIO_MODULE_DIR
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)
endif ()
install (TARGETS elektrasettings LIBRARY DESTINATION ${GIO_MODULE_DIR})

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	if (NOT FORCE_IN_SOURCE_BUILD)
		message(FATAL_ERROR
				"In-source builds are not permitted.\n"
				"Make a separate folder for building:\n"
				"    mkdir build && cd build && cmake ..\n"
				"Before that, remove the files already created:\n"
				"    rm -rf CMakeCache.txt CMakeFiles\n"
				"If you really know what you are doing\n"
				"(will overwrite original files!) use:\n"
				"    cmake -DFORCE_IN_SOURCE_BUILD=ON\n"
			)
	endif (NOT FORCE_IN_SOURCE_BUILD)
endif(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
