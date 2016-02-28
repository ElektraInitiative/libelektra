include (LibAddPlugin)

generate_manpage (elektra-plugins FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md SECTION 7)

set (COLLECTION_PHASE ON)
set (DEPENDENCY_PHASE OFF)
set (ADDTESTING_PHASE OFF)

file (GLOB DIRECTORIES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/*)

foreach (plugin ${DIRECTORIES})
	if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${plugin})
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



#TODO:
# - symlink for default plugin instead of having it in code?
#
#	shouldn't that be at installation time??
#	mkdir (${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
#	create_symlink (
#			libelektra-${BACKEND_DEFAULT}.so
#			${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}/libelektra-default.so
#			)
#	message (STATUS "Default backend: ${BACKEND_DEFAULT}")
