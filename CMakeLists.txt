cmake_minimum_required (VERSION 2.8.8)

# allows us to override platform specific variables
set(CMAKE_USER_MAKE_RULES_OVERRIDE "${CMAKE_SOURCE_DIR}/cmake/Platform.cmake")

project (Elektra)

#fix OS X RPATH issues
set(CMAKE_MACOSX_RPATH 1)

#additional modules for loading libraries
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

#fix policy stuff
if (POLICY CMP0026)
	cmake_policy(SET CMP0026 OLD) # reinvestigate at 2.8.11 or later
endif()
if (POLICY CMP0043)
	cmake_policy(SET CMP0043 OLD) # to have consistent behavior for pre 2.8.10
	#also reinvestigate when they do not need to be supported anymore
endif()
if (POLICY CMP0046)
	cmake_policy(SET CMP0046 OLD) # new behaviour seems stupid, of course
	# dependency is not yet available if it's generated later?
endif()



#needed by ifs below
include (cmake/ElektraCache.cmake)
include (cmake/ElektraCompiling.cmake)
if (ENABLE_COVERAGE)
	include (cmake/ElektraCoverage.cmake)
endif (ENABLE_COVERAGE)

if (ENABLE_TESTING)
	enable_testing ()
	include (Dart)
endif (ENABLE_TESTING)


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

if (NOT CMAKE_BUILD_TYPE)
	SET(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif (NOT CMAKE_BUILD_TYPE)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


#some basic and important variables
set (DOMAIN libelektra.org) #needed by doxygen
set (REVERSE_DOMAIN org.libelektra)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 15)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}")

set (SO_VERSION 4)
#set (SO_VERSION_TOOLS 3)

#needs version above
include (cmake/ElektraPackaging.cmake)

#now add all directories
add_subdirectory (cmake)
add_subdirectory (scripts)

if (BUILD_DOCUMENTATION)
	add_subdirectory (doc)
endif (BUILD_DOCUMENTATION)

#is there anything to build except documentation?
if (BUILD_FULL OR BUILD_STATIC OR BUILD_SHARED)
	if (BUILD_TESTING)
		find_package (GTest)
	endif (BUILD_TESTING)

	add_subdirectory (src)
	add_subdirectory (examples)
	add_subdirectory (benchmarks)

	if (BUILD_TESTING)
		add_subdirectory (tests)
	endif (BUILD_TESTING)
endif()

