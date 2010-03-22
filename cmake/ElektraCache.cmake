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

option (SHARED_BUILD "Should the shared version of libelektra be build?" ON)
option (FULL_BUILD "Should libelektra-full be build?" ON)
option (STATIC_BUILD "Should libelektra-static be build?" ON)

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
