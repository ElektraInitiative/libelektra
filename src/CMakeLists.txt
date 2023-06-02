set (ADDTESTING_PHASE OFF)

include_directories ("${CMAKE_CURRENT_SOURCE_DIR}/include" "${CMAKE_CURRENT_BINARY_DIR}/include")

# ~~~
# RPATH settings, see http://www.itk.org/Wiki/CMake_RPATH_handling
# those settings are needed to find the plugins at runtime even without ld.so.conf
# search only in the prefix the user decided to install elektra
# (e.g. /usr/local/)
# ~~~
set (ELEKTRA_RPATH "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}")

add_subdirectory (plugins)

add_subdirectory (libs)

add_subdirectory (bindings)

add_subdirectory (tools)

add_subdirectory (include)

if (BUILD_TESTING)
	set (ADDTESTING_PHASE ON)
	add_subdirectory (plugins plugins_tests)
endif (BUILD_TESTING)
