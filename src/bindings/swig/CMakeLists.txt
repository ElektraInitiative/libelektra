if (BUILD_SWIG_PYTHON2)
	add_subdirectory (python2)
endif (BUILD_SWIG_PYTHON2)

if (BUILD_SWIG_PYTHON3)
	add_subdirectory (python3)
endif (BUILD_SWIG_PYTHON3)

if (BUILD_SWIG_LUA)
	add_subdirectory (lua)
endif (BUILD_SWIG_LUA)
