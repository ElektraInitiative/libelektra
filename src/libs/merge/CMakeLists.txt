include (LibAddMacros)
find_package (LibGit2 0.24.1 QUIET)
if (LibGit2_FOUND)
	set (CMERGE_INCLUDE_DIRS ${LibGit2_INCLUDE_DIRS})
	set (CMERGE_LIBRARY_DIRS ${LibGit2_LIBRARIES})
	add_definitions (-DLIBGITFOUND)
endif ()

# elektra-ease is required for elektraArrayIncName and elektraArrayValidateName see add_plugin for how LINK_ELEKTRA etc. work
#
# we have to link CMERGE_LIBRARY_DIRS here so that elektra-merge.so has the functions from libgit2.so
add_lib (
	merge
	SOURCES
	kdbmerge.c
	INCLUDE_SYSTEM_DIRECTORIES
	${CMERGE_INCLUDE_DIRS}
	LINK_ELEKTRA
	elektra-ease
	LINK_LIBRARIES
	${CMERGE_LIBRARY_DIRS}
	COMPONENT
	elektra-bin)

set (LIBRARY_NAME elektra-merge)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc" @ONLY)

install (
	FILES "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc"
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	COMPONENT libelektra-dev)
