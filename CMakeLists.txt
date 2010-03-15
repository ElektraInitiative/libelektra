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

set (BACKENDS filesys hosts fstab)

set (KDB_DB_SYSTEM "/etc")

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

add_definitions (-DHAVE_CONFIG_H)

add_subdirectory(src)
