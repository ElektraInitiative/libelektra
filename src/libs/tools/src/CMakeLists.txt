include (LibAddMacros)

add_headers(HDR_FILES)
add_cppheaders(HDR_FILES)
add_toolheaders(HDR_FILES)

file (GLOB_RECURSE SRC_FILES *.cpp)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

set (__symbols_file ${CMAKE_CURRENT_SOURCE_DIR}/libelektratools-symbols.map)

if (BUILD_SHARED)
	add_library (elektratools SHARED ${SOURCES})
	add_dependencies (elektratools kdberrors_generated)

	target_link_libraries (elektratools elektra-core elektra-kdb elektra-plugin elektra-ease elektra-meta)

	set_target_properties (elektratools PROPERTIES
		COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_SHARED"
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION_TOOLS}
		CLEAN_DIRECT_OUTPUT 1
		LINKER_LANGUAGE CXX
	)

	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektratools PROPERTIES
			LINK_FLAGS "-Wl,--version-script=${__symbols_file}"
		)
	endif ()

	install (TARGETS elektratools DESTINATION lib${LIB_SUFFIX})
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_library (elektratools-full SHARED ${SOURCES})
	add_dependencies (elektratools-full kdberrors_generated)

	target_link_libraries (elektratools-full elektra-full)

	set_target_properties (elektratools-full PROPERTIES
		COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION_TOOLS}
		CLEAN_DIRECT_OUTPUT 1
		LINKER_LANGUAGE CXX
	)

	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektratools-full PROPERTIES
			LINK_FLAGS "-Wl,--version-script=${__symbols_file}"
		)
	endif ()

	install (TARGETS elektratools-full DESTINATION lib${LIB_SUFFIX})
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_library (elektratools-static STATIC ${SOURCES})
	add_dependencies (elektratools-static kdberrors_generated)

	target_link_libraries (elektratools-static elektra-static)

	set_target_properties (elektratools-static PROPERTIES
		COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION_TOOLS}
		CLEAN_DIRECT_OUTPUT 1
		LINKER_LANGUAGE CXX
	)

	set_target_properties (elektratools-static PROPERTIES LINKER_LANGUAGE CXX)
	if (${LD_ACCEPTS_VERSION_SCRIPT})
		set_target_properties (elektratools-static PROPERTIES
			LINK_FLAGS "-Wl,--version-script=${__symbols_file}"
		)
	endif()

	install (TARGETS elektratools-static DESTINATION lib${LIB_SUFFIX})
endif (BUILD_STATIC)
