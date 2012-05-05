#
# CACHE
#
# Here the cache variables are set

set (PLUGINS dump resolver CACHE STRING "Which plugins should be compiled?")

# May be changed to /etc/config when XDG will be implemented
set (KDB_DB_SYSTEM "/etc/kdb" CACHE PATH
		"The path to the system key database."
		)

set (KDB_DB_HOME "/home" CACHE PATH
		"The path to users home directories."
		)

# May be changed to .config when XDG will be implemented
set (KDB_DB_USER ".kdb" CACHE PATH
		"This path will be appended after the resolved home directory. It completes the path to the user key database."
		)

option (BUILD_SHARED "Build the shared version of elektra." ON)
option (BUILD_FULL "Build the full version of elektra (shared with all selected backends included)." ON)
option (BUILD_STATIC "Build the static version of elektra (all selected backends included statically)." ON)

option (BUILD_EXAMPLES "Build example applications using elektra." ON)

option (BUILD_DOCUMENTATION "Build the documentation (API, man pages)" ON)
if (BUILD_DOCUMENTATION)
	option (INSTALL_DOCUMENTATION "Install the documentation (API, man pages)" ON)
else (BUILD_DOCUMENTATION)
	#install documentation makes no sense if it is not build
	#(even though the option would not harm)
	set (INSTALL_DOCUMENTATION OFF CACHE BOOL
			"Install the documentation (API, man pages)"
			FORCE
		)
endif (BUILD_DOCUMENTATION)

option (ENABLE_TESTING "Enable to run tests by make test target" ON)
option (BUILD_TESTING "Build testcases" ON)
if (BUILD_TESTING)
	option (INSTALL_TESTING "Install testcases" ON)
elseif (BUILD_TESTING)
	#install testing makes no sense if it is not build
	#(even though the option would not harm)
	set (INSTALL_TESTING OFF CACHE BOOL "Install testcases" FORCE)
endif (BUILD_TESTING)

option (ELEKTRA_DEBUG_BUILD "Build with extra debug print messages (to debug elektra).")
if (ELEKTRA_DEBUG_BUILD)
	set (DEBUG "1")
else (ELEKTRA_DEBUG_BUILD)
	set (DEBUG "0")
endif (ELEKTRA_DEBUG_BUILD)

option (ELEKTRA_VERBOSE_BUILD "Build with even more print messages (to debug elektra).")
if (ELEKTRA_VERBOSE_BUILD)
	set (VERBOSE "1")
else (ELEKTRA_VERBOSE_BUILD)
	set (VERBOSE "0")
endif (ELEKTRA_VERBOSE_BUILD)

set (CMAKE_DESTINATION
		"${CMAKE_INSTALL_PREFIX}/share/cmake-2.6/Modules"
		CACHE PATH
		"Where to install cmake files?"
    )

set (PKGCONFIG_DESTINATION
		"${CMAKE_INSTALL_PREFIX}/lib/pkgconfig"
		CACHE PATH
		"Where to install pkgconfig files?"
    )

set (DOCUMENTATION_DESTINATION
		"${CMAKE_INSTALL_PREFIX}/share/doc/elektra-api"
		CACHE PATH
		"Where to install documentation files?"
    )

set (MEMORYCHECK_COMMAND
		/usr/bin/valgrind
		CACHE FILEPATH
		"Path to valgrind the memory checker"
    )
