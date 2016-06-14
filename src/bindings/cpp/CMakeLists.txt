include (LibAddMacros)

if (BUILD_TESTING)
	add_subdirectory (tests)
endif (BUILD_TESTING)
add_subdirectory (examples)
add_subdirectory (benchmarks)

add_subdirectory(include)
