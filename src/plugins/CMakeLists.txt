include (LibAddPlugin)

if (ADDTESTING_PHASE)
	set (COLLECTION_PHASE OFF)
	set (DEPENDENCY_PHASE OFF)
	set (ADDTESTING_PHASE ON)

	foreach (plugin ${ADDED_DIRECTORIES})
		set (CMAKE_CURRENT_SOURCE_DIR_TOPSAFE ${CMAKE_CURRENT_SOURCE_DIR})
		set (CMAKE_CURRENT_BINARY_DIR_TOPSAFE ${CMAKE_CURRENT_BINARY_DIR})
		set (PLUGIN_DIRECTORY_NAME ${plugin})
		set (CMAKE_CURRENT_SOURCE_DIR "${CMAKE_SOURCE_DIR}/src/plugins/${plugin}")
		set (CMAKE_CURRENT_BINARY_DIR "${CMAKE_BINARY_DIR}/src/plugins/${plugin}")
		include ("${plugin}/CMakeLists.txt")
		set (CMAKE_CURRENT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR_TOPSAFE})
		set (CMAKE_CURRENT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR_TOPSAFE})
	endforeach (plugin)
	return ()
endif ()

generate_man_page (
	elektra-plugins
	FILENAME
	${CMAKE_CURRENT_SOURCE_DIR}/README.md
	SECTION
	7
	COMPONENT
	elektra-doc)

set (COLLECTION_PHASE ON)
set (DEPENDENCY_PHASE OFF)
set (ADDTESTING_PHASE OFF)

file (
	GLOB DIRECTORIES
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/*)

foreach (plugin ${DIRECTORIES})

	# message (STATUS ${plugin})
	if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${plugin} AND NOT ${plugin} STREQUAL CMakeFiles)
		set (CMAKE_CURRENT_SOURCE_DIR_TOPSAFE ${CMAKE_CURRENT_SOURCE_DIR})
		set (CMAKE_CURRENT_BINARY_DIR_TOPSAFE ${CMAKE_CURRENT_BINARY_DIR})
		set (PLUGIN_DIRECTORY_NAME ${plugin})
		set (CMAKE_CURRENT_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/${plugin}")
		set (CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/${plugin}")
		include ("${plugin}/CMakeLists.txt")
		set (CMAKE_CURRENT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR_TOPSAFE})
		set (CMAKE_CURRENT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR_TOPSAFE})
	endif ()
endforeach (plugin)

#
# Now ADDED_DIRECTORIES and ADDED_PLUGINS is modified accordingly
#

set (COLLECTION_PHASE OFF)
set (DEPENDENCY_PHASE ON)
set (ADDTESTING_PHASE OFF)

foreach (plugin ${ADDED_DIRECTORIES})
	if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${plugin})
		set (PLUGIN_DIRECTORY_NAME ${plugin})
		add_subdirectory ("${plugin}")
	else ()
		message (WARNING "${plugin} is not a valid directory")
	endif ()
endforeach (plugin)

list (FIND ADDED_PLUGINS ${KDB_DEFAULT_STORAGE} output)
if (output EQUAL -1)
	message (
		SEND_ERROR
			"selected default storage (${KDB_DEFAULT_STORAGE}) is not selected in PLUGINS, please change KDB_DEFAULT_STORAGE or PLUGINS"
	)
endif ()

list (FIND ADDED_PLUGINS ${KDB_DEFAULT_RESOLVER} output)
if (output EQUAL -1)
	message (
		SEND_ERROR
			"selected default resolver (${KDB_DEFAULT_RESOLVER}) is not selected in PLUGINS, please change KDB_DEFAULT_RESOLVER or PLUGINS"
	)
endif ()

list (FIND ADDED_PLUGINS spec output)
if (output EQUAL -1
    AND (NOT MINGW)
    AND ENABLE_TESTING
    AND BUILD_TESTING)
	message (SEND_ERROR "The required plugin `spec` is not selected in PLUGINS but needed for testing, please add `spec` to PLUGINS")
endif ()

if (BUILD_SHARED)
	mkdir (${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
	create_lib_symlink (libelektra-${KDB_DEFAULT_STORAGE}.so libelektra-storage.so libelektra${SO_VERSION} PLUGIN)

	create_lib_symlink (libelektra-${KDB_DEFAULT_RESOLVER}.so libelektra-resolver.so libelektra${SO_VERSION} PLUGIN)
endif (BUILD_SHARED)
