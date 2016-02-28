if (DEPENDENCY_PHASE)
	try_compile(HAS_UNAME
			"${CMAKE_BINARY_DIR}"
			"${PROJECT_SOURCE_DIR}/src/plugins/uname/testuname.c"
			)

	include(LibAddMacros)

	if (HAS_UNAME)
		add_plugintest (uname)
	else()
		remove_plugin (uname "uname is missing")
	endif()
endif ()

add_plugin (uname
	SOURCES
		uname.h
		uname.c
)
