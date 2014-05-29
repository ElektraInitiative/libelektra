include (LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

add_subdirectory(include)
add_subdirectory(src)

if (BUILD_FULL OR BUILD_STATIC)
	add_subdirectory (tests)
	add_subdirectory (examples)
endif (BUILD_FULL OR BUILD_STATIC)

