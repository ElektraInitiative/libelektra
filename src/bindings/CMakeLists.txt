add_subdirectory (cpp)

if (BUILD_SWIG)
	find_package(SWIG 2 REQUIRED)
endif (BUILD_SWIG)

if (SWIG_FOUND)
	add_subdirectory (swig)
endif (SWIG_FOUND)
