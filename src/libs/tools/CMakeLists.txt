add_subdirectory (include)
add_subdirectory (src)
add_subdirectory (examples)

if (ENABLE_TESTING)
	if (BUILD_FULL OR BUILD_STATIC)
		add_subdirectory (tests)
	endif (BUILD_FULL OR BUILD_STATIC)
endif(ENABLE_TESTING)

