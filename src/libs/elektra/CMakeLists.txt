# do not include plugins configured with SHARED_ONLY in the exported symbols as those are only used for the FULL and STATIC builds
get_property (SHARED_ONLY_PLUGINS GLOBAL PROPERTY SHARED_ONLY_PLUGINS)
set (ADDED_PLUGINS_WITHOUT_ONLY_SHARED ${ADDED_PLUGINS})
if (SHARED_ONLY_PLUGINS)
	list (REMOVE_ITEM ADDED_PLUGINS_WITHOUT_ONLY_SHARED ${SHARED_ONLY_PLUGINS})
endif (SHARED_ONLY_PLUGINS)

include (ExportPluginSymbols)
elektra_export_plugin_symbols (
	SOURCE
	${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.c
	HEADER
	${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.h
	RESOLVER
	${KDB_DEFAULT_RESOLVER}
	STORAGE
	${KDB_DEFAULT_STORAGE}
	PLUGINS
	${ADDED_PLUGINS_WITHOUT_ONLY_SHARED})

# Include the shared header files of the Elektra project
include (LibAddMacros)
add_headers (HDR_FILES)

# include the current binary directory to get exported_symbols.h
include_directories ("${CMAKE_CURRENT_BINARY_DIR}")

# now add all source files of this folder
file (GLOB SRC_FILES *.c)

# remove those we don't want..
file (GLOB RM_FILES kdbmainpage.c)
unset (RM_LOG_FILE)
if (HAVE_LOGGER)
	file (GLOB RM_LOG_FILE nolog.c)
else (HAVE_LOGGER)
	file (GLOB RM_LOG_FILE log.c)
endif (HAVE_LOGGER)
list (REMOVE_ITEM SRC_FILES ${RM_FILES} ${RM_LOG_FILE})

# remove the opmphm files
if (NOT ENABLE_OPTIMIZATIONS)
	file (GLOB OPMPHM_FILES opmphm*.c)
	list (REMOVE_ITEM SRC_FILES ${OPMPHM_FILES})
endif (NOT ENABLE_OPTIMIZATIONS)

# now add all source files of other folders
get_property (elektra_SRCS GLOBAL PROPERTY elektra_SRCS)
list (APPEND SRC_FILES ${elektra_SRCS})

set (SOURCES ${SRC_FILES} ${HDR_FILES})
list (APPEND SOURCES "${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.h")

# the targets built to export
set (targets_built)

set (__symbols_file "${CMAKE_BINARY_DIR}/src/libs/symbols.map")

if (BUILD_SHARED)
	file (
		GLOB
		KDB_FILES
		kdb.c
		cmount.c
		hooks.c
		backends.c
		plugin.c
		contracts.c)
	set (CORE_FILES ${SOURCES})
	list (REMOVE_ITEM CORE_FILES ${KDB_FILES})
	set (KDB_FILES ${KDB_FILES} ${HDR_FILES})

	get_property (elektra-shared_SRCS GLOBAL PROPERTY elektra-shared_SRCS)
	add_library (elektra-core SHARED ${CORE_FILES} ${elektra-shared_SRCS})
	add_dependencies (elektra-core generate_version_script)

	get_property (elektra-shared_LIBRARIES GLOBAL PROPERTY elektra-shared_LIBRARIES)
	target_link_libraries (elektra-core ${elektra-shared_LIBRARIES})

	get_property (elektra-shared_INCLUDES GLOBAL PROPERTY elektra-shared_INCLUDES)
	include_directories (${elektra-shared_INCLUDES})

	add_library (elektra-kdb SHARED ${KDB_FILES})
	add_dependencies (elektra-kdb generate_version_script)
	target_link_libraries (elektra-kdb elektra-core)

	get_property (elektra-extension_LIBRARIES GLOBAL PROPERTY elektra-extension_LIBRARIES)

	set (elektra-all_LIBRARIES elektra-core elektra-kdb ${elektra-extension_LIBRARIES})
	set_target_properties (
		${elektra-all_LIBRARIES}
		PROPERTIES COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H"
			   VERSION ${KDB_VERSION}
			   SOVERSION ${SO_VERSION}
			   CLEAN_DIRECT_OUTPUT 1
			   LINK_DEPENDS "${__symbols_file}")

	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (${elektra-all_LIBRARIES} PROPERTIES LINK_FLAGS "-Wl,--version-script='${__symbols_file}'")
	endif ()

	# ~~~
	# RPATH settings, see http://www.itk.org/Wiki/CMake_RPATH_handling
	# those settings are needed to find the plugins at runtime even without ld.so.conf
	# search only in the prefix the user decided to install elektra
	# (e.g. /usr/local/)
	# ~~~
	set (ELEKTRA_RPATH "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}")
	set_target_properties (${elektra-all_LIBRARIES} PROPERTIES INSTALL_RPATH "${ELEKTRA_RPATH}")

	install (
		TARGETS elektra-core elektra-kdb
		DESTINATION lib${LIB_SUFFIX}
		EXPORT ElektraTargetsLibelektra
		COMPONENT libelektra${SO_VERSION})

	list (APPEND targets_built ${elektra-all_LIBRARIES})
endif (BUILD_SHARED)

if (BUILD_FULL OR BUILD_STATIC)

	# for the next two builds, append some more sources
	get_property (elektra-full_SRCS GLOBAL PROPERTY elektra-full_SRCS)
	list (APPEND SOURCES ${elektra-full_SRCS} ${BINARY_INCLUDE_DIR}/kdberrors.h)
	list (APPEND SOURCES "${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.c")

	# and get all libraries to link against
	get_property (elektra-full_LIBRARIES GLOBAL PROPERTY elektra-full_LIBRARIES)

	# include the directories of all libraries for the static or full-shared build
	get_property (elektra-full_INCLUDES GLOBAL PROPERTY elektra-full_INCLUDES)

	include_directories (${elektra-full_INCLUDES})
endif (BUILD_FULL OR BUILD_STATIC)

if (BUILD_FULL)
	set_source_files_properties (${PROJECT_BINARY_DIR}/src/error/elektra_errors.c PROPERTIES GENERATED ON)
	add_library (elektra-full SHARED ${SOURCES})
	add_dependencies (elektra-full generate_version_script)

	target_link_libraries (elektra-full ${elektra-full_LIBRARIES})

	set_target_properties (
		elektra-full
		PROPERTIES COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
			   VERSION ${KDB_VERSION}
			   SOVERSION ${SO_VERSION}
			   CLEAN_DIRECT_OUTPUT 1
			   LINKER_LANGUAGE CXX
			   LINK_DEPENDS "${__symbols_file}")
	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektra-full PROPERTIES LINK_FLAGS "-Wl,--version-script='${__symbols_file}'")
	endif ()

	install (
		TARGETS elektra-full
		DESTINATION lib${LIB_SUFFIX}
		EXPORT ElektraTargetsLibelektra
		COMPONENT libelektra${SO_VERSION}-full)

	list (APPEND targets_built elektra-full)
endif (BUILD_FULL)

if (BUILD_STATIC)
	set_source_files_properties (${PROJECT_BINARY_DIR}/src/error/elektra_errors.c PROPERTIES GENERATED ON)
	add_library (elektra-static STATIC ${SOURCES})
	add_dependencies (elektra-static generate_version_script)

	target_link_libraries (elektra-static ${elektra-full_LIBRARIES})

	set_target_properties (
		elektra-static
		PROPERTIES COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
			   VERSION ${KDB_VERSION}
			   SOVERSION ${SO_VERSION}
			   CLEAN_DIRECT_OUTPUT 1
			   LINKER_LANGUAGE CXX
			   LINK_DEPENDS "${__symbols_file}")
	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektra-static PROPERTIES LINK_FLAGS "-Wl,--version-script='${__symbols_file}'")
	endif ()

	install (
		TARGETS elektra-static
		DESTINATION lib${LIB_SUFFIX}
		EXPORT ElektraTargetsLibelektra
		COMPONENT libelektra${SO_VERSION})

	list (APPEND targets_built elektra-static)
endif (BUILD_STATIC)

export (TARGETS ${targets_built} FILE "${CMAKE_CURRENT_BINARY_DIR}/ElektraTargetsLibelektra.cmake")
install (
	EXPORT ElektraTargetsLibelektra
	DESTINATION "${TARGET_CMAKE_FOLDER}"
	COMPONENT libelektra-dev)

# if you want the same name for those 3 libraries, here is how to do it:
# http://www.itk.org/Wiki/CMake_FAQ#How_do_I_make_my_shared_and_static_libraries_have_the_same_root_name.2C_but_different_suffixes.3F
