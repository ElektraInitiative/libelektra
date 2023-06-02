include (LibAddLib)

set (
	SUBDIRS
	core
	kdb
	type
	ease
	globbing
	meta
	plugin
	pluginprocess
	utility
	io
	invoke
	notification
	highlevel
	merge
	opts)

set (SYM_MAPS "")
foreach (subdir ${SUBDIRS})
	list (APPEND SYM_MAPS "${subdir}/symbols.map")
endforeach (subdir)

# tools has special symbol handling
list (INSERT SUBDIRS 0 tools)

# object libraries -> don't have symbol.map
list (INSERT SUBDIRS 0 base pluginload)

add_custom_command (
	OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/symbols.map"
	COMMAND awk 1 ${SYM_MAPS} | awk -v vfile=versions.def -f version_gen.awk > "${CMAKE_CURRENT_BINARY_DIR}/symbols.map"
	WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
	DEPENDS version_gen.awk versions.def ${SYM_MAPS}
	COMMENT "Creating version script"
	VERBATIM)
add_custom_target (generate_version_script DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/symbols.map")

foreach (subdir ${SUBDIRS})
	add_subdirectory ("${subdir}")
endforeach (subdir)

generate_man_page (
	elektra-libs
	FILENAME
	"${CMAKE_CURRENT_SOURCE_DIR}/README.md"
	SECTION
	7
	COMPONENT
	elektra-doc)

# ##########################################################################################################################################
# Handling of BUILD_FULL AND BUILD_STATIC #
# ##########################################################################################################################################

# do not include plugins configured with SHARED_ONLY in the exported symbols as those are only used for the FULL and STATIC builds
get_property (SHARED_ONLY_PLUGINS GLOBAL PROPERTY SHARED_ONLY_PLUGINS)
set (ADDED_PLUGINS_WITHOUT_ONLY_SHARED ${ADDED_PLUGINS})
if (SHARED_ONLY_PLUGINS)
	list (REMOVE_ITEM ADDED_PLUGINS_WITHOUT_ONLY_SHARED ${SHARED_ONLY_PLUGINS})
endif (SHARED_ONLY_PLUGINS)

include (ExportPluginSymbols)
elektra_export_plugin_symbols (
	SOURCE
	"${CMAKE_BINARY_DIR}/src/exported_symbols.c"
	HEADER
	"${CMAKE_BINARY_DIR}/src/include/internal/exported_symbols.h"
	RESOLVER
	${KDB_DEFAULT_RESOLVER}
	STORAGE
	${KDB_DEFAULT_STORAGE}
	PLUGINS
	${ADDED_PLUGINS_WITHOUT_ONLY_SHARED})

# collect all the source files
get_property (elektra_SRCS GLOBAL PROPERTY elektra_SRCS)

set (SOURCES ${elektra_SRCS} ${HDR_FILES})
list (APPEND SOURCES "${CMAKE_BINARY_DIR}/src/include/internal/exported_symbols.h")

# the targets built to export
set (targets_built)

set (__symbols_file "${CMAKE_BINARY_DIR}/src/libs/symbols.map")

if (BUILD_FULL OR BUILD_STATIC)
	# for the next two builds, append some more sources
	get_property (elektra-full_SRCS GLOBAL PROPERTY elektra-full_SRCS)
	list (APPEND SOURCES "${CMAKE_BINARY_DIR}/src/exported_symbols.c")

	# and get all libraries to link against
	get_property (elektra-full_LIBRARIES GLOBAL PROPERTY elektra-full_LIBRARIES)

	# include the directories of all libraries for the static or full-shared build
	get_property (elektra-full_INCLUDES GLOBAL PROPERTY elektra-full_INCLUDES)
endif (BUILD_FULL OR BUILD_STATIC)

if (BUILD_FULL)
	set_source_files_properties (${PROJECT_BINARY_DIR}/src/error/elektra_errors.c PROPERTIES GENERATED ON)
	add_library (elektra-full SHARED ${SOURCES})
	add_dependencies (elektra-full generate_version_script)

	target_link_libraries (elektra-full ${elektra-full_LIBRARIES})
	# include the current binary directory to get exported_symbols.h
	target_include_directories (elektra-full PRIVATE ${elektra-full_INCLUDES} "${CMAKE_CURRENT_BINARY_DIR}")

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
	# include the current binary directory to get exported_symbols.h
	target_include_directories (elektra-static PRIVATE ${elektra-full_INCLUDES} "${CMAKE_CURRENT_BINARY_DIR}")

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
