cmake_minimum_required (VERSION 2.6)

project (Elektra)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 0)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}")
set (KDB_VERSION_MAJORMINOR "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}")

set (PACKAGE_NAME "Elektra")
set (PACKAGE_URL "http://www.libelektra.org/")
set (PACKAGE_BUGREPORT "http://bugs.libelektra.org/")

#
# CACHE
#

set (BACKENDS filesys hosts fstab CACHE STRING "Which backends to use?")

set (KDB_DB_SYSTEM "/etc" CACHE PATH "Where should be the system configuration?")

option (DEBUG_BUILD "Build with extra debug print messages.")
if (DEBUG_BUILD)
	set (DEBUG "1")
else (DEBUG_BUILD)
	set (DEBUG "0")
endif (DEBUG_BUILD)

option (VERBOSE_BUILD "Build with even more print messages.")
if (VERBOSE_BUILD)
	set (VERBOSE "1")
else (VERBOSE_BUILD)
	set (VERBOSE "0")
endif (VERBOSE_BUILD)

set (PROJECT_CMAKE_DIR
		"${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules"
		CACHE PATH
		"Where to install cmake files?"
       )

#
# Now do the real work
#

add_subdirectory(src)
add_subdirectory(cmake)

