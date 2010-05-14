include (LibAddMacros)

if (BUILD_FULL OR BUILD_STATIC)
	add_subdirectory (tests)
endif (BUILD_FULL OR BUILD_STATIC)
