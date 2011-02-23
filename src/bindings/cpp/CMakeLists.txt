include (LibAddMacros)

if (BUILD_FULL OR BUILD_STATIC)
	add_subdirectory (tests)
	add_subdirectory (examples)
endif (BUILD_FULL OR BUILD_STATIC)

add_subdirectory(include)
