include (LibAddMacros)

add_headers(HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

if (BUILD_SHARED)
	add_library (elektragetenv SHARED ${SOURCES})

	target_link_libraries (elektragetenv ${WRAP} elektra)

	set_property(TARGET elektragetenv
		APPEND PROPERTY COMPILE_FLAGS
		${WRAP}
		)

	install (TARGETS elektragetenv DESTINATION lib${LIB_SUFFIX})
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_library (elektragetenv-full SHARED ${SOURCES})

	target_link_libraries (elektragetenv-full ${WRAP} elektra-full)

	set_property(TARGET elektragetenv-full
		APPEND PROPERTY COMPILE_FLAGS
		${WRAP}
		)

	install (TARGETS elektragetenv-full DESTINATION lib${LIB_SUFFIX})
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_library (elektragetenv-static STATIC ${SOURCES})

	set_target_properties (elektragetenv-static PROPERTIES LINKER_LANGUAGE CXX)

	set_property(TARGET elektragetenv-static
		APPEND PROPERTY COMPILE_FLAGS
		${WRAP}
		)

	target_link_libraries (elektragetenv-static ${WRAP} elektra-static)

	install (TARGETS elektragetenv-static DESTINATION lib${LIB_SUFFIX})
endif (BUILD_STATIC)
