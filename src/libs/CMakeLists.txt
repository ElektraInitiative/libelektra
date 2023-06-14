include (LibAddLib)

set (
	LIBRARIES
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
foreach (lib ${LIBRARIES})
	list (APPEND SYM_MAPS "${lib}/symbols.map")
endforeach (lib)

add_custom_command (
	OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/symbols.map"
	COMMAND awk 1 ${SYM_MAPS} | awk -v vfile=versions.def -f version_gen.awk > "${CMAKE_CURRENT_BINARY_DIR}/symbols.map"
	WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
	DEPENDS version_gen.awk versions.def ${SYM_MAPS}
	COMMENT "Creating version script"
	VERBATIM)
add_custom_target (generate_version_script DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/symbols.map")

# object libraries don't have symbol.map
add_subdirectory (base)
add_subdirectory (pluginload)

# tools has special symbol handling
add_subdirectory (tools)

foreach (lib ${LIBRARIES})
	add_subdirectory ("${lib}")
endforeach (lib)

generate_man_page (
	elektra-libs
	FILENAME
	"${CMAKE_CURRENT_SOURCE_DIR}/README.md"
	SECTION
	7
	COMPONENT
	elektra-doc)

set (targets_built)
if (BUILD_FULL OR BUILD_STATIC)
	get_property (SHARED_ONLY_PLUGINS GLOBAL PROPERTY SHARED_ONLY_PLUGINS)
	get_property (EXTERNAL_LIBRARIES GLOBAL PROPERTY ELEKTRA_LINK_LIBRARIES)

	# do not include plugins configured with SHARED_ONLY in the exported symbols as those are only used for the FULL and STATIC builds
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

	set (__symbols_file "${CMAKE_BINARY_DIR}/src/libs/symbols.map")

	set (OBJ_TARGETS $<TARGET_OBJECTS:elektra-base> $<TARGET_OBJECTS:elektra-pluginload-static>)
	foreach (lib ${ADDED_LIBRARIES})
		list (APPEND OBJ_TARGETS "$<TARGET_OBJECTS:elektra-${lib}-objects>")
	endforeach (lib)
	foreach (plugin ${ADDED_PLUGINS_WITHOUT_ONLY_SHARED})
		list (APPEND OBJ_TARGETS "$<TARGET_OBJECTS:elektra-${plugin}-objects>")
	endforeach (plugin)
	if (TARGET io-adapter-dbus)
		list (APPEND OBJ_TARGETS "$<TARGET_OBJECTS:io-adapter-dbus>")
	endif ()
	if (TARGET io-adapter-zeromq)
		list (APPEND OBJ_TARGETS "$<TARGET_OBJECTS:io-adapter-zeromq>")
	endif ()

	if (BUILD_FULL)
		add_library (elektra-full SHARED "${CMAKE_BINARY_DIR}/src/exported_symbols.c" ${OBJ_TARGETS})
		add_dependencies (elektra-full generate_version_script)

		target_link_libraries (elektra-full ${EXTERNAL_LIBRARIES})

		set_target_properties (
			elektra-full
			PROPERTIES COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
				   VERSION ${KDB_VERSION}
				   SOVERSION ${SO_VERSION}
				   CLEAN_DIRECT_OUTPUT 1
				   LINKER_LANGUAGE C
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
		add_library (elektra-static STATIC "${CMAKE_BINARY_DIR}/src/exported_symbols.c" ${OBJ_TARGETS})
		add_dependencies (elektra-static generate_version_script)

		target_link_libraries (elektra-static ${EXTERNAL_LIBRARIES})

		set_target_properties (
			elektra-static
			PROPERTIES COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
				   VERSION ${KDB_VERSION}
				   SOVERSION ${SO_VERSION}
				   CLEAN_DIRECT_OUTPUT 1
				   LINKER_LANGUAGE C
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

endif (BUILD_FULL OR BUILD_STATIC)

if (BUILD_SHARED)
	foreach (lib ${ADDED_LIBRARIES})
		list (APPEND targets_built "elektra-${lib}")

		# TODO: should only be required for libs that load plugin (i.e. kdb and plugin)
		set_target_properties (elektra-${lib} PROPERTIES INSTALL_RPATH "${ELEKTRA_RPATH}")
	endforeach (lib)
endif (BUILD_SHARED)

export (TARGETS ${targets_built} FILE "${CMAKE_CURRENT_BINARY_DIR}/ElektraTargetsLibelektra.cmake")
install (
	EXPORT ElektraTargetsLibelektra
	DESTINATION "${TARGET_CMAKE_FOLDER}"
	COMPONENT libelektra-dev)

# if you want the same name for those 3 libraries, here is how to do it:
# http://www.itk.org/Wiki/CMake_FAQ#How_do_I_make_my_shared_and_static_libraries_have_the_same_root_name.2C_but_different_suffixes.3F
