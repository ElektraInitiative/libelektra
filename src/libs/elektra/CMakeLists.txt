# For static inclusion of libraries we need a list
# of symbols to make the static runtime linker work.
#
# For that there is the program elektra-export-symbols which
# generates the sourcefiles exported_symbols.h
# and exported_symbols.c containing an array of
# all symbols.
#
# The program needs a list of all backends as
# argument.
#
# Symbols of libelektratools will be treaded specially.
add_executable (elektra-export-symbols exportsymbols.c)

if (INSTALL_BUILD_TOOLS)
	install (TARGETS elektra-export-symbols DESTINATION bin)
endif (INSTALL_BUILD_TOOLS)

include(LibAddMacros)
find_util(elektra-export-symbols EXE_SYM_LOC EXE_SYM_ARG)


set (ARG ${KDB_DEFAULT_RESOLVER} ${KDB_DEFAULT_STORAGE} ${ADDED_PLUGINS})
add_custom_command (
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.h
		DEPENDS elektra-export-symbols
		COMMAND ${EXE_SYM_LOC}
		ARGS ${EXE_SYM_ARG} ${ARG}
		)

add_custom_command (
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.c
		DEPENDS elektra-export-symbols
		COMMAND ${EXE_SYM_LOC}
		ARGS ${EXE_SYM_ARG} ${ARG}
		)


# Include the shared header files of the elektra project
include(LibAddMacros)
add_headers(HDR_FILES)

#include the current binary directory to get exported_symbols.h
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

#now add all source files of this folder
file (GLOB SRC_FILES *.c)

#remove those we don't want..
file (GLOB RM_FILES exportsymbols.c kdbmainpage.c)
list (REMOVE_ITEM SRC_FILES ${RM_FILES} ${WIN32_RM_FILES})

#remove the opmphm files
if (NOT ENABLE_OPTIMIZATIONS)
	file (GLOB OPMPHM_FILES opmphm*.c)
	list (REMOVE_ITEM SRC_FILES ${OPMPHM_FILES})
endif (NOT ENABLE_OPTIMIZATIONS)

#now add all source files of other folders
get_property (elektra_SRCS GLOBAL PROPERTY elektra_SRCS)
list (APPEND SRC_FILES ${elektra_SRCS})

set (SOURCES ${SRC_FILES} ${HDR_FILES})
list (APPEND SOURCES "${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.h")

#the targets built to export
set (targets_built)

SET(__symbols_file ${CMAKE_CURRENT_SOURCE_DIR}/libelektra-symbols.map)

if (BUILD_SHARED)
	file (GLOB KDB_FILES backend.c  kdb.c   mount.c  split.c  trie.c  plugin.c)
	set (CORE_FILES ${SOURCES})
	list (REMOVE_ITEM CORE_FILES ${KDB_FILES})
	set (KDB_FILES  ${KDB_FILES}  ${HDR_FILES})

	get_property (elektra-shared_SRCS GLOBAL PROPERTY elektra-shared_SRCS)
	add_library (elektra-core SHARED ${CORE_FILES} ${elektra-shared_SRCS})
	add_dependencies (elektra-core kdberrors_generated)

	get_property (elektra-shared_LIBRARIES GLOBAL PROPERTY elektra-shared_LIBRARIES)
	target_link_libraries (elektra-core ${elektra-shared_LIBRARIES})

	get_property (elektra-shared_INCLUDES GLOBAL PROPERTY elektra-shared_INCLUDES)
	include_directories (${elektra-shared_INCLUDES})




	add_library (elektra-kdb SHARED ${KDB_FILES})
	add_dependencies (elektra-kdb kdberrors_generated)
	target_link_libraries (elektra-kdb elektra-core)




	#message(STATUS "ignore the following ADD_LIBRARY warning")
	#add_library (elektra INTERFACE) # no SOVERSION?
	add_library (elektra SHARED  ${KDB_FILES} ${CORE_FILES}  ${elektra-shared_SRCS})
	add_dependencies (elektra kdberrors_generated)
	get_property (elektra-extension_LIBRARIES GLOBAL PROPERTY elektra-extension_LIBRARIES)
	target_link_libraries (elektra ${elektra-shared_LIBRARIES})
	#target_link_libraries (elektra ${elektra-extension_LIBRARIES})
	#target_link_libraries (elektra elektra-core elektra-kdb)
	#set_target_properties (${elektra-all_LIBRARIES} PROPERTIES LINK_FLAGS "--copy-dt-needed-entries")
	set_target_properties (elektra PROPERTIES LINKER_LANGUAGE C)




	set (elektra-all_LIBRARIES elektra-core elektra-kdb elektra ${elektra-extension_LIBRARIES})
	set_target_properties (${elektra-all_LIBRARIES} PROPERTIES
		COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H"
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION}
		CLEAN_DIRECT_OUTPUT 1
	)

	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (${elektra-all_LIBRARIES} PROPERTIES
			LINK_FLAGS "-Wl,--version-script=${__symbols_file}"
		)
	endif ()

	#RPATH settings, see http://www.itk.org/Wiki/CMake_RPATH_handling
	#those settings are needed to find the plugins at runtime even without ld.so.conf
	#search only in the prefix the user decided to install elektra
	#(e.g. /usr/local/)
	set (ELEKTRA_RPATH "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}")
	set_target_properties (${elektra-all_LIBRARIES} PROPERTIES INSTALL_RPATH "${ELEKTRA_RPATH}")

	install (TARGETS elektra-core elektra-kdb elektra DESTINATION lib${LIB_SUFFIX} EXPORT ElektraTargetsLibelektra)

	list (APPEND targets_built ${elektra-all_LIBRARIES})
endif (BUILD_SHARED)

if (BUILD_FULL OR BUILD_STATIC)
	#for the next two builds, append some more sources
	get_property (elektra-full_SRCS GLOBAL PROPERTY elektra-full_SRCS)
	list (APPEND SOURCES ${elektra-full_SRCS}
		${BINARY_INCLUDE_DIR}/kdberrors.h)
	list (APPEND SOURCES "${CMAKE_CURRENT_BINARY_DIR}/exported_symbols.c")

	#and get all libraries to link against
	get_property (elektra-full_LIBRARIES GLOBAL PROPERTY elektra-full_LIBRARIES)

	# include the directories of all libraries for the static or
	# full-shared build
	get_property (elektra-full_INCLUDES GLOBAL PROPERTY elektra-full_INCLUDES)

	include_directories (${elektra-full_INCLUDES})
endif (BUILD_FULL OR BUILD_STATIC)


if (BUILD_FULL)
	add_library (elektra-full SHARED ${SOURCES})
	add_dependencies (elektra-full kdberrors_generated)

	target_link_libraries (elektra-full ${elektra-full_LIBRARIES})

	set_target_properties (elektra-full PROPERTIES
		COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION}
		CLEAN_DIRECT_OUTPUT 1
		LINKER_LANGUAGE CXX
	)
	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektra-full PROPERTIES
			LINK_FLAGS "-Wl,--version-script=${__symbols_file}"
		)
	endif ()

	install (TARGETS elektra-full DESTINATION lib${LIB_SUFFIX} EXPORT ElektraTargetsLibelektra)

	list (APPEND targets_built elektra-full)
endif (BUILD_FULL)


if (BUILD_STATIC)
	add_library (elektra-static STATIC ${SOURCES})
	add_dependencies (elektra-static kdberrors_generated)

	target_link_libraries (elektra-static ${elektra-full_LIBRARIES})

	set_target_properties (elektra-static PROPERTIES
		COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION}
		CLEAN_DIRECT_OUTPUT 1
		LINKER_LANGUAGE CXX
	)
	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektra-static PROPERTIES
			LINK_FLAGS "-Wl,--version-script=${__symbols_file}"
		)
	endif ()

	install (TARGETS elektra-static DESTINATION lib${LIB_SUFFIX} EXPORT ElektraTargetsLibelektra)

	list (APPEND targets_built elektra-static)
endif (BUILD_STATIC)

export (TARGETS ${targets_built} FILE "${CMAKE_CURRENT_BINARY_DIR}/ElektraTargetsLibelektra.cmake")
install (EXPORT ElektraTargetsLibelektra DESTINATION "${TARGET_CMAKE_FOLDER}")

#if you want the same name for those 3 libraries, here is how to do it:
#http://www.itk.org/Wiki/CMake_FAQ#How_do_I_make_my_shared_and_static_libraries_have_the_same_root_name.2C_but_different_suffixes.3F
