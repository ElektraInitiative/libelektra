include (LibAddMacros)
if (DEPENDENCY_PHASE)
	find_package (LibGit2 0.24.1 QUIET)
	if (NOT LibGit2_FOUND)
		remove_plugin (gitresolver "Cannot find libgit2 >= 0.24.1")
	endif ()
	find_package (LibOpenSSL)
	if (NOT OPENSSL_FOUND)
		remove_plugin (gitresolver "OpenSSL-dev not found")
	endif ()
	if (OPENSSL_FOUND AND LibGit2_FOUND)
		set (GITRESOLVER_INCLUDE_DIRS ${LibGit2_INCLUDE_DIRS} ${OPENSSL_INCLUDE_DIR})
		set (GITRESOLVER_LIBRARY_DIRS ${LibGit2_LIBRARIES} ${OPENSSL_LIBRARIES})
	endif ()

endif ()
add_plugin (
	gitresolver
	SOURCES gitresolver.h gitresolver.c
	LINK_ELEKTRA elektra-invoke
	INCLUDE_SYSTEM_DIRECTORIES ${GITRESOLVER_INCLUDE_DIRS}
	LINK_LIBRARIES ${GITRESOLVER_LIBRARY_DIRS} COMPONENT libelektra${SO_VERSION}-gitresolver)
