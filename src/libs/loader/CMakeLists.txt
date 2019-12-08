include (LibAddMacros)

add_sources (elektra-full "static.c")

if (BUILD_SHARED)
	add_sources (elektra-shared "dl.c")
	add_includes (elektra-shared)

	if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
		set (BUILD_ARGS -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH} -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
				-DCMAKE_C_FLAGS=${CMAKE_C_FLAGS})

		if (CMAKE_TOOLCHAIN_FILE)
			list (APPEND BUILD_ARGS -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE})
		endif ()

		include (ExternalProject)
		externalproject_add (
			dlfcn-win32
			PREFIX "dlfcn-win32"
			GIT_REPOSITORY "https://github.com/dlfcn-win32/dlfcn-win32.git"
			CMAKE_ARGS ${BUILD_ARGS}
			BUILD_IN_SOURCE 1)

		add_includes (elektra-shared "${CMAKE_CURRENT_BINARY_DIR}/dlfcn-win32/include")
		set (CMAKE_DL_LIBS "${CMAKE_CURRENT_BINARY_DIR}/dlfcn-win32/lib/libdl.dll.a")
	endif ()
	add_libraries (elektra-shared ${CMAKE_DL_LIBS})
endif (BUILD_SHARED)
