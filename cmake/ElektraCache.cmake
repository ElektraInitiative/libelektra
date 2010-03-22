#
# CACHE
#
# Here the cache variables are set

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

option (BUILD_SHARED "Build the shared version of elektra." ON)
option (BUILD_FULL "Build the full version of elektra (shared with all selected backends included)." ON)
option (BUILD_STATIC "Build the static version of elektra (all selected backends included)." ON)

option (BUILD_TOOLS "Build the library libelektratools (shared, full and static options take affect here too)" ON)

set (PROJECT_CMAKE_DIR
		"${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules"
		CACHE PATH
		"Where to install cmake files?"
       )

set (MEMORYCHECK_COMMAND
		/usr/bin/valgrind
		CACHE FILEPATH
		"Path to valgrind the memory checker"
    )
