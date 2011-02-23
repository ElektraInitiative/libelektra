set (PACKAGE_NAME "elektra")
set (CPACK_PACKAGE_NAME "${PACKAGE_NAME}")
set (PACKAGE_URL "http://www.libelektra.org/")
set (PACKAGE_BUGREPORT "http://bugs.libelektra.org/")

#see http://public.kitware.com/Bug/view.php?id=7000
SET(CPACK_SET_DESTDIR "ON")


set (PROJECT_VERSION "${KDB_VERSION}")
set (CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set (CPACK_PACKAGE_DESCRIPTION_SUMMARY 
"Elektra is a universal hierarchical configuration store, with related goals like GConf and the Windows Registry. It allows programs to read and save their configurations with a consistent API, and allows them to be aware of other applications' configurations, leveraging easy application integration. The whole point of it is to tie applications together, so that they can co-operate and share their user-preferences."
	)
set (CPACK_PACKAGE_CONTACT "${PACKAGE_URL}")
set (CPACK_SOURCE_IGNORE_FILES
	"/.cvsignore"
	"/.gitignore"
	"/songs/"
	"/build/"
	"/.svn/"
	"/.git/"
	"/osx-utils/"
	"/portage-overlay/"
)
set (CPACK_PACKAGE_EXECUTABLES kdb)
set (CPACK_SOURCE_GENERATOR "TBZ2")
set (CPACK_GENERATOR "TBZ2")

if ("${CMAKE_BUILD_TYPE}" MATCHES "Release")
	set (CPACK_STRIP_FILES TRUE)
endif ("${CMAKE_BUILD_TYPE}" MATCHES "Release")

if (APPLE)
	set (CPACK_GENERATOR "PACKAGEMAKER;OSXX11")
endif (APPLE)
if (UNIX)
	# Try to find architecture
	execute_process (COMMAND uname -m OUTPUT_VARIABLE CPACK_PACKAGE_ARCHITECTURE)
	string (STRIP "${CPACK_PACKAGE_ARCHITECTURE}" CPACK_PACKAGE_ARCHITECTURE)

	# Try to find distro name and distro-specific arch
	execute_process (COMMAND lsb_release -is OUTPUT_VARIABLE LSB_ID)
	execute_process (COMMAND lsb_release -rs OUTPUT_VARIABLE LSB_RELEASE)
	string (STRIP "${LSB_ID}" LSB_ID)
	string (STRIP "${LSB_RELEASE}" LSB_RELEASE)
	set (LSB_DISTRIB "${LSB_ID}${LSB_RELEASE}")
	if (NOT LSB_DISTRIB)
		set (LSB_DISTRIB "unix")
	endif (NOT LSB_DISTRIB)

	# For Debian-based distros we want to create DEB packages.
	if ("${LSB_DISTRIB}" MATCHES "Ubuntu|Debian")
		set (CPACK_GENERATOR "DEB")
		set (CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
		set (CPACK_DEBIAN_PACKAGE_SECTION "libs")
		set (CPACK_DEBIAN_PACKAGE_RECOMMENDS "")

		# We need to alter the architecture names as per distro rules
		if ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "i[3-6]86")
			set (CPACK_PACKAGE_ARCHITECTURE i386)
		endif ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "i[3-6]86")
		if ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "x86_64")
			set (CPACK_PACKAGE_ARCHITECTURE amd64)
		endif ("${CPACK_PACKAGE_ARCHITECTURE}" MATCHES "x86_64")

		# Set the dependencies based on the distro version
		# thus only one package is build you must list here *any* depending libraries,
		# even if they are only used in one module
		if ("${LSB_DISTRIB}" MATCHES "Debian5.*")
			set (CPACK_DEBIAN_PACKAGE_DEPENDS "libxml2-dev")
		endif ("${LSB_DISTRIB}" MATCHES "Debian5.*")
		if (NOT CPACK_DEBIAN_PACKAGE_DEPENDS)
			message ("WARNING: ${LSB_DISTRIB} not supported yet.\nPlease set deps in cmake/ElektraPackaging.cmake before packaging.")
		endif (NOT CPACK_DEBIAN_PACKAGE_DEPENDS)
	endif ("${LSB_DISTRIB}" MATCHES "Ubuntu|Debian")
	set (CPACK_SYSTEM_NAME "${LSB_DISTRIB}-${CPACK_PACKAGE_ARCHITECTURE}")
	message (STATUS "Detected ${CPACK_SYSTEM_NAME}. Use make package to build packages (${CPACK_GENERATOR}).")
endif (UNIX)

include (CPack)

