include (${SWIG_USE_FILE})
include(LibAddMacros)

find_package (PythonInterp 2.7 REQUIRED)
find_package (PythonLibs 2.7 REQUIRED)

if (PYTHONLIBS_VERSION_STRING MATCHES "^3\\.[0-9]+")
	message (SEND_ERROR "python2 is for python 2.7 only")
endif ()

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

include_directories (${PYTHON_INCLUDE_DIR})
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

set (CMAKE_SWIG_OUTDIR ${CMAKE_CURRENT_BINARY_DIR})
set (CMAKE_SWIG_FLAGS "-O")

set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
set_source_files_properties (kdb.i PROPERTIES SWIG_FLAGS "-extranative")
swig_add_module (swig-python2 python kdb.i)
swig_link_libraries (swig-python2 elektra ${PYTHON_LIBRARIES})
set_target_properties (_swig-python2 PROPERTIES OUTPUT_NAME _kdb)

set (PYTHON_GET_MODULES_DIR_COMMAND "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
execute_process (
	COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
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
