if (BUILD_SWIG_PYTHON)
	add_subdirectory (python)
endif (BUILD_SWIG_PYTHON)

if (BUILD_SWIG_LUA)
	add_subdirectory (lua)
endif (BUILD_SWIG_LUA)
