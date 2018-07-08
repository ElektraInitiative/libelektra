add_subdirectory (include)
add_subdirectory (src)
add_subdirectory (examples)
add_subdirectory (benchmarks)

if (ENABLE_TESTING)
	add_subdirectory (tests)
endif (ENABLE_TESTING)
