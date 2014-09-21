add_subdirectory (cpp)

if (BUILD_SWIG_PYTHON2 OR BUILD_SWIG_PYTHON3 OR BUILD_SWIG_LUA)
	find_package(SWIG 2 REQUIRED)
	add_subdirectory (swig)
endif ()

if (BUILD_GLIB)
	set (GELEKTRA_LIBRARY gelektra-${SO_VERSION}.0)
	add_subdirectory (glib)

	if (BUILD_GLIB_GI)
		add_subdirectory (gi)
	endif (BUILD_GLIB_GI)
endif (BUILD_GLIB)
