include (LibAddMacros)

if (BUILD_FULL OR BUILD_STATIC)
	add_sources (elektra-full "static.c")
endif (BUILD_FULL OR BUILD_STATIC)

if (BUILD_SHARED)
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

		set (CMAKE_DL_LIBS "${CMAKE_CURRENT_BINARY_DIR}/dlfcn-win32/lib/libdl.dll.a")

		add_library (elektra-pluginload OBJECT ${CMAKE_DL_LIBS})
		target_include_directories (elektra-pluginload SYSTEM PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/dlfcn-win32/include")
	else ()
		add_library (elektra-pluginload OBJECT dl.c)
	endif ()

	set_property (TARGET elektra-pluginload PROPERTY POSITION_INDEPENDENT_CODE ON)
endif (BUILD_SHARED)
