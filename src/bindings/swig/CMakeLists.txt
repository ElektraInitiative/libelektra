if (BUILD_SWIG_PYTHON)
	find_package (PythonInterp 2.7 REQUIRED)
	find_package (PythonLibs 2.7 REQUIRED)

	if (PYTHONLIBS_VERSION_STRING MATCHES "^3\\.[0-9]+")
		add_subdirectory (python)
	else ()
		add_subdirectory (python-legacy)
	endif ()
endif (BUILD_SWIG_PYTHON)

if (BUILD_SWIG_LUA)
	add_subdirectory (lua)
endif (BUILD_SWIG_LUA)
