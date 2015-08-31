include (${SWIG_USE_FILE})
include(LibAddMacros)

#set (PythonInterp_FIND_VERSION_EXACT ON)
find_package (Python2Interp 2.7)
find_package (Python2Libs 2.7)

if (NOT PYTHON2INTERP_FOUND)
	remove_binding(swig_python2 "python2 interpreter not found")
elseif (NOT PYTHON2LIBS_FOUND)
	remove_binding(swig_python2 "python2 libs not found")
elseif (PYTHON2LIBS_VERSION_STRING MATCHES "^3\\.[0-9]+")
	remove_binding(swig_python2 "python2 is for python 2.7 only and not ${PYTHON2LIBS_VERSION_STRING}")
else()
	message(STATUS "Include Binding swig_python2")
	if (${SWIG_VERSION} MATCHES "^2\\.")
		message(WARNING "Python bindings might crash if generated with SWIG 2."
				" Please upgrade to SWIG 3 or later.")
	endif()

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)

	include_directories (${PYTHON2_INCLUDE_DIR})
	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O")

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
	set_source_files_properties (kdb.i PROPERTIES SWIG_FLAGS "-extranative")
	swig_add_module (swig-python2 python kdb.i)
	swig_link_libraries (swig-python2 elektra ${PYTHON2_LIBRARIES})
	set_target_properties (_swig-python2 PROPERTIES OUTPUT_NAME _kdb)

	set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	set (PYTHON_GET_MODULES_DIR_COMMAND "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
	execute_process (
		COMMAND ${PYTHON2_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
		OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)

	install (
		FILES ${CMAKE_CURRENT_BINARY_DIR}/kdb.py
		DESTINATION ${PYTHON_SITE_PACKAGES}
	)

	install (
		TARGETS _swig-python2
		LIBRARY DESTINATION ${PYTHON_SITE_PACKAGES}
	)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

	add_subdirectory (tests)
endif()
