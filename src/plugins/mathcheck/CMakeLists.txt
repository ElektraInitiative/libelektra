include (LibAddMacros)

if (NOT CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
	add_plugin (mathcheck
		SOURCES
			mathcheck.h
			mathcheck.c
			floathelper.h
			floathelper.c
		)

	add_plugintest (mathcheck)
endif ()
