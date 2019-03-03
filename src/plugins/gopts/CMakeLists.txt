include (LibAddMacros)

if (WIN32)
	set (gopts_source gopts_win32.h)
elseif (MacOSX)
	set (gopts_source gopts_osx.h)
else ()
	try_compile (SYSCTL_TEST ${CMAKE_CURRENT_BINARY_DIR} "${CMAKE_SOURCE_DIR}/src/plugins/gopts/sysctl_test.c")

	if (SYSCTL_TEST)
		set (gopts_source gopts_sysctl.h)
	elseif (EXISTS "/proc/mounts")
		set (gopts_source gopts_procfs.h)
	endif ()
endif ()

if (gopts_source)
	add_plugin (gopts
		    SOURCES gopts.h
			    gopts.c
			    ${gopts_source}
		    LINK_ELEKTRA elektra-opts
		    ADD_TEST
		    TEST_README)
endif ()
