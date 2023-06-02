include (LibAddMacros)

if (WIN32)
	set (ELEKTRA_GOPTS_WIN32 ON)
	set (gopts_source gopts_win32.h)
elseif (APPLE)
	set (ELEKTRA_GOPTS_OSX ON)
	set (gopts_source gopts_osx.h)
else ()
	try_compile (
		SYSCTL_TEST ${CMAKE_CURRENT_BINARY_DIR}
		"${CMAKE_SOURCE_DIR}/src/plugins/gopts/sysctl_test.c"
		OUTPUT_VARIABLE SYSCTL_TEST_OUTPUT)

	if (SYSCTL_TEST)
		set (ELEKTRA_GOPTS_SYSCTL ON)
		set (gopts_source gopts_sysctl.h)
	elseif (EXISTS "/proc/mounts")
		set (ELEKTRA_GOPTS_PROCFS ON)
		set (gopts_source gopts_procfs.h)
	else ()
		message (STATUS "gopts: Failed to find one of the supported platforms. Supported are the Win32 API, the OSX API, the "
				"sysctl API (with support for KERN_PROC_ARGS) or a mounted procfs.")
		message ("${SYSCTL_TEST_OUTPUT}")
	endif ()
endif ()

if (gopts_source)
	if (DEPENDENCY_PHASE)
		message (STATUS "gopts: using ${gopts_source}")
	endif ()
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/gopts_impl.c.in" "${CMAKE_CURRENT_BINARY_DIR}/gopts_impl.c" @ONLY)

	add_plugin (
		gopts
		SOURCES gopts.h gopts.c ${gopts_source}
		LINK_ELEKTRA elektra-opts
		TEST_README COMPONENT libelektra${SO_VERSION}-experimental)

	if (ADDTESTING_PHASE)
		set (TEST_SOURCES $<TARGET_OBJECTS:cframework> ${ARG_OBJECT_SOURCES})

		add_executable (elektra-gopts-testapp "${CMAKE_CURRENT_SOURCE_DIR}/testapp.c" ${TEST_SOURCES})
		target_link_elektra (elektra-gopts-testapp elektra-core elektra-invoke elektra-opts)
		target_include_directories (elektra-gopts-testapp PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")

		set (TESTAPP_NAME "elektra-gopts-testapp")
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/config.c.in" "${CMAKE_CURRENT_BINARY_DIR}/config.c" @ONLY)

		add_plugintest (
			gopts
			INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR} EXTRA_EXECUTABLES elektra-gopts-testapp
			TIMEOUT 240)

		if (BUILD_SHARED)
			add_dependencies (elektra-gopts-testapp elektra-gopts)
		endif (BUILD_SHARED)
		add_dependencies (testmod_gopts elektra-gopts-testapp)
	endif ()
elseif (DEPENDENCY_PHASE)
	remove_plugin ("could not determine implementation")
endif ()
