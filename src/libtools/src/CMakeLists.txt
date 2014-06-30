include (LibAddMacros)

add_headers(HDR_FILES)
add_cppheaders(HDR_FILES)
add_toolheaders(HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.cpp)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

if (BUILD_SHARED)
	add_library (elektratools SHARED ${SOURCES})

	target_link_libraries (elektratools elektra)

	install (TARGETS elektratools DESTINATION lib${LIB_SUFFIX})
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_library (elektratools-full SHARED ${SOURCES})

	target_link_libraries (elektratools-full elektra-full)

	install (TARGETS elektratools-full DESTINATION lib${LIB_SUFFIX})
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_library (elektratools-static STATIC ${SOURCES})

	set_target_properties (elektratools-static PROPERTIES LINKER_LANGUAGE CXX)

	target_link_libraries (elektratools-static elektra-static)

	install (TARGETS elektratools-static DESTINATION lib${LIB_SUFFIX})
endif (BUILD_STATIC)
