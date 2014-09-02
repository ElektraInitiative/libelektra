add_subdirectory (cpp)

if (BUILD_SWIG_PYTHON2 OR BUILD_SWIG_PYTHON3 OR BUILD_SWIG_LUA)
	find_package(SWIG 2 REQUIRED)
	add_subdirectory (swig)
endif ()
