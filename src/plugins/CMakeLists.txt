include (LibAddPlugin)

generate_manpage (elektra-plugins FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md SECTION 7)

set (COLLECTION_PHASE ON)
set (DEPENDENCY_PHASE OFF)
set (ADDTESTING_PHASE OFF)

file (GLOB DIRECTORIES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/*)

foreach (plugin ${DIRECTORIES})
	#message (STATUS ${plugin})
	if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${plugin} AND NOT ${plugin} STREQUAL CMakeFiles)
		set (PLUGIN_DIRECTORY_NAME ${plugin})
		include ("${plugin}/CMakeLists.txt")
	endif ()
endforeach(plugin)

#
# Now ADDED_DIRECTORIES and ADDED_PLUGINS is modified accordingly
#

list (SORT ADDED_DIRECTORIES)
list (REMOVE_DUPLICATES ADDED_DIRECTORIES)


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
endforeach(plugin)



list (FIND ADDED_PLUGINS ${KDB_DEFAULT_STORAGE} output)
if (output EQUAL -1)
	message(SEND_ERROR "selected default storage (${KDB_DEFAULT_STORAGE})  is not selected in PLUGINS, please change KDB_DEFAULT_STORAGE or PLUGINS")
endif()

list (FIND ADDED_PLUGINS ${KDB_DEFAULT_RESOLVER} output)
if (output EQUAL -1)
	message(SEND_ERROR "selected default resolver (${KDB_DEFAULT_RESOLVER}) is not selected in PLUGINS, please change KDB_DEFAULT_RESOLVER or PLUGINS")
endif()

mkdir (${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
create_lib_symlink (
		libelektra-${KDB_DEFAULT_STORAGE}.so
		libelektra-storage.so
		)

create_lib_symlink (
		libelektra-${KDB_DEFAULT_RESOLVER}.so
		libelektra-resolver.so
		)
