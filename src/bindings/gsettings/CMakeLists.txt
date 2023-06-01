# TODO: why a separate project?
project (
	elektrasettings
	VERSION 0.1.1
	LANGUAGES C)

find_package (PkgConfig QUIET)

if (NOT PKG_CONFIG_FOUND)
	exclude_binding (gsettings "Need PkgConfig needed to find dependencies for gsettings backend")
	return ()
endif ()

pkg_check_modules (GLIB glib-2.0>=2.42 QUIET)
pkg_check_modules (GMODULE gmodule-2.0>=2.42 QUIET)
pkg_check_modules (GIO gio-2.0>=2.42 QUIET)
if (NOT GLIB_FOUND)
	exclude_binding (gsettings "glib >= 2.42 needed for gsettings backend")
	return ()
endif ()
if (NOT GMODULE_FOUND)
	exclude_binding (gsettings "gmodule >= 2.42 needed for gsettings backend")
	return ()
endif ()
if (NOT GIO_FOUND)
	exclude_binding (gsettings "gio >= 2.42 needed for gsettings backend")
	return ()
endif ()

check_binding_was_added (glib IS_GLIB_INCLUDED)
if (NOT IS_GLIB_INCLUDED)
	exclude_binding (gsettings "gsettings depends on glib")
	return ()
endif ()

add_binding (gsettings)

include_directories (${GLIB_INCLUDE_DIRS})
add_library (elektrasettings SHARED elektrasettingsbackend.c ${ELEKTRA_HEADERS})
set (
	GSETTINGS_MODULE_PRIORITY
	50
	CACHE STRING "GIO Module Priority. Higher than 100 means the Elektra gsettings backend will be default.")
target_compile_definitions (elektrasettings PRIVATE G_ELEKTRA_SETTINGS_MODULE_PRIORITY=${GSETTINGS_MODULE_PRIORITY})
include_directories (${CMAKE_SOURCE_DIR}/src/include)
if (GELEKTRA_LIBRARY)
	pkg_get_variable (GIO_MODULE_DIR gio-2.0 giomoduledir)
	add_dependencies (elektrasettings elektra_config_headers)
	target_link_libraries (elektrasettings ${GLIB_LIBRARIES} ${GMODULE_LIBRARIES} ${GIO_LIBRARIES} ${GELEKTRA_LIBRARY} elektra-core)
	if (INSTALL_SYSTEM_FILES)
		install (TARGETS elektrasettings LIBRARY DESTINATION ${GIO_MODULE_DIR})
	endif ()
else ()
	pkg_get_variable (GIO_MODULE_DIR gio-2.0 giomoduledir)
	pkg_check_modules (GELEKTRA gelektra-4.0>=0.8.16 QUIET)
	if (!GELEKTRA_FOUND)
		remove_binding (gsettings "elektra glib bindings needed for gsettings backend")
	endif ()
	target_link_libraries (elektrasettings ${GLIB_LIBRARIES} ${GMODULE_LIBRARIES} ${GIO_LIBRARIES} ${GELEKTRA_LIBRARIES})
	install (TARGETS elektrasettings LIBRARY DESTINATION ${GIO_MODULE_DIR})
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
