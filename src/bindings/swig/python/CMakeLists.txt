include (${SWIG_USE_FILE})
include (LibAddMacros)

# set (PythonInterp_FIND_VERSION_EXACT ON)
find_package (PythonInterp 3 QUIET)
find_package (PythonLibs 3 QUIET)

if (NOT PYTHONINTERP_FOUND)
	exclude_binding (swig_python "python3 interpreter not found")
elseif (NOT PYTHONLIBS_FOUND)
	exclude_binding (swig_python "python3 libs not found")
elseif (PYTHONLIBS_VERSION_STRING MATCHES "^2\\.[0-9]+")
	exclude_binding (swig_python "python3 is for python 3 only and not ${PYTHONLIBS_VERSION_STRING}")
else ()
	if (NOT BUILD_SHARED)
		exclude_binding (swig_python "it can only be built if `BUILD_SHARED` is enabled")
		return ()
	endif (NOT BUILD_SHARED)

	add_binding (swig_python)

	if (${SWIG_VERSION} MATCHES "^2\\.")
		message (WARNING "Python bindings might crash if generated with SWIG 2." " Please upgrade to SWIG 3 or later.")
	endif ()

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)

	include_directories (${PYTHON_INCLUDE_DIRS})
	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O")

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
	set_source_files_properties (kdb.i PROPERTIES SWIG_FLAGS "-py3;-extranative")
	if (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_module (swig-python python kdb.i)
	else (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_library (
			swig-python
			LANGUAGE
			python
			SOURCES
			kdb.i
			TYPE
			MODULE)
	endif (CMAKE_VERSION VERSION_LESS 3.8)
	swig_link_libraries (swig-python elektra-core elektra-kdb ${PYTHON_LIBRARIES})
	set_target_properties (_swig-python PROPERTIES OUTPUT_NAME _kdb)

	# Unfortunately the generated code produces warnings.
	if (CMAKE_COMPILER_IS_GNUCXX)
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-cast-function-type")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-stringop-truncation")
	endif (CMAKE_COMPILER_IS_GNUCXX)
	set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	set (PYTHON_GET_MODULES_DIR_COMMAND
	     "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
	execute_process (
		COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
		OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
		OUTPUT_STRIP_TRAILING_WHITESPACE)

	install (FILES ${CMAKE_CURRENT_BINARY_DIR}/kdb.py DESTINATION ${PYTHON_SITE_PACKAGES})

	install (TARGETS _swig-python LIBRARY DESTINATION ${PYTHON_SITE_PACKAGES})

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

	add_subdirectory (tests)
endif ()
