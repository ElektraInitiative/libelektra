include (${SWIG_USE_FILE})
include (LibAddMacros)

set (PYTHON2_VERSION 2.7)

# set (PythonInterp_FIND_VERSION_EXACT ON)
find_package (Python2Interp ${PYTHON2_VERSION} QUIET)

# Make sure we use the correct Python version on macOS
if (APPLE AND PYTHON2INTERP_FOUND)
	execute_process (COMMAND ${PYTHON2_EXECUTABLE}-config --prefix
			 OUTPUT_VARIABLE PYTHON2_LIBRARY_PREFIX
			 OUTPUT_STRIP_TRAILING_WHITESPACE)
	set (PYTHON2_LIBRARY
	     ${PYTHON2_LIBRARY_PREFIX}/lib/libpython${PYTHON2_VERSION}${CMAKE_SHARED_LIBRARY_SUFFIX}
	     CACHE FILEPATH "Filepath of the Python 2 library")
	set (PYTHON2_INCLUDE_DIR
	     ${PYTHON2_LIBRARY_PREFIX}/include/python${PYTHON2_VERSION_MAJOR}.${PYTHON2_VERSION_MINOR}
	     CACHE PATH "Path to the Python 2 include directory")
endif (APPLE AND PYTHON2INTERP_FOUND)

find_package (Python2Libs ${PYTHON2_VERSION} QUIET)

if (NOT PYTHON2INTERP_FOUND)
	exclude_binding (swig_python2 "python2 interpreter not found")
elseif (NOT PYTHON2LIBS_FOUND)
	exclude_binding (swig_python2 "python2 libs not found")
elseif (PYTHON2LIBS_VERSION_STRING MATCHES "^3\\.[0-9]+")
	exclude_binding (swig_python2 "python2 is for python 2.7 only and not ${PYTHON2LIBS_VERSION_STRING}")
else ()
	if (NOT BUILD_SHARED)
		exclude_binding (swig_python2 "it can only be built if `BUILD_SHARED` is enabled")
		return ()
	endif (NOT BUILD_SHARED)

	add_binding (swig_python2)

	if (${SWIG_VERSION} MATCHES "^2\\.")
		message (WARNING "Python bindings might crash if generated with SWIG 2." " Please upgrade to SWIG 3 or later.")
	endif ()

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)

	include_directories (${PYTHON2_INCLUDE_DIRS})
	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O")

	set_source_files_properties (kdb.i
				     PROPERTIES
				     CPLUSPLUS
				     ON)
	set_source_files_properties (kdb.i
				     PROPERTIES
				     SWIG_FLAGS
				     "-extranative")
	if (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_module (swig-python2 python kdb.i)
	else (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_library (swig-python2
				  LANGUAGE
				  python
				  SOURCES
				  kdb.i
				  TYPE
				  MODULE)
	endif (CMAKE_VERSION VERSION_LESS 3.8)
	swig_link_libraries (swig-python2
			     elektra-core
			     elektra-kdb
			     ${PYTHON2_LIBRARIES})
	set_target_properties (_swig-python2 PROPERTIES OUTPUT_NAME _kdb)

	# Unfortunately compiling the generated code produces warnings.
	if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-deprecated-register")
	elseif (CMAKE_COMPILER_IS_GNUCXX)
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-cast-function-type")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-stringop-truncation")
	endif ()

	set_source_files_properties (${swig_generated_file_fullname}
				     PROPERTIES
				     COMPILE_FLAGS
				     "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	set (PYTHON_GET_MODULES_DIR_COMMAND
	     "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
	execute_process (COMMAND ${PYTHON2_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
			 OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
			 OUTPUT_STRIP_TRAILING_WHITESPACE)

	install (FILES ${CMAKE_CURRENT_BINARY_DIR}/kdb.py DESTINATION ${PYTHON_SITE_PACKAGES})

	install (TARGETS _swig-python2 LIBRARY DESTINATION ${PYTHON_SITE_PACKAGES})

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

	add_subdirectory (tests)
endif ()
