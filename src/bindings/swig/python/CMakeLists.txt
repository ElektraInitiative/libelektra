# ~~~
# Disable warnings about unset CMake policy.
# TODO: Remove the calls to `cmake_policy` after we upgraded to new behavior.
# See also: http://issues.libelektra.org/3330
# ~~~
if (POLICY CMP0078)
	cmake_policy (PUSH)
	cmake_policy (SET CMP0078 OLD)
endif (POLICY CMP0078)

if (POLICY CMP0086)
	cmake_policy (SET CMP0086 NEW)
endif (POLICY CMP0086)

include (${SWIG_USE_FILE})
include (LibAddMacros)

# set (PythonInterp_FIND_VERSION_EXACT ON)
find_package (PythonInterp 3.4 QUIET)
find_package (PythonLibs 3.4 QUIET)

if (NOT PYTHONINTERP_FOUND)
	exclude_binding (python "python3 interpreter not found")
elseif (NOT PYTHONLIBS_FOUND)
	exclude_binding (python "python3 libs not found")
elseif (PYTHONLIBS_VERSION_STRING MATCHES "^2\\.[0-9]+")
	exclude_binding (python "python3 is for python 3 only and not ${PYTHONLIBS_VERSION_STRING}")
elseif (SWIG_VERSION VERSION_LESS "4.0.2" AND (PYTHONLIBS_VERSION_STRING VERSION_GREATER "3.8.0" OR PYTHONLIBS_VERSION_STRING VERSION_EQUAL
												    "3.8.0"))
	exclude_binding (python "SWIG <= 4.0.1 incompatible with Python >= 3.8")
elseif (NOT BUILD_SHARED)
	exclude_binding (python "it can only be built if `BUILD_SHARED` is enabled")
else ()
	add_binding (python)

	if (${SWIG_VERSION} MATCHES "^2\\.")
		message (WARNING "Python bindings might crash if generated with SWIG 2." " Please upgrade to SWIG 3 or later.")
	endif ()

	include_directories (SYSTEM ${PYTHON_INCLUDE_DIRS})
	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/include" "${CMAKE_SOURCE_DIR}/src/libs/tools/include")

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}/kdb")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O;-DSWIG_WITHOUT_OVERRIDE")

	# specify the SWIG_TYPE_TABLE to use (has to be in sync with the python plugin)
	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	# TODO: Reenable the following warning after we add a virtual destructor to `PluginDatabase`, and its subclasses. See also:
	#
	# - https://github.com/ElektraInitiative/libelektra/pull/1841
	# - https://github.com/ElektraInitiative/libelektra/commit/15d67328
	#
	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-delete-non-virtual-dtor")
	# Unfortunately the generated code produces warnings.
	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-unused-parameter")
	if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-cast-function-type")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-stringop-truncation")
	endif ()

	macro (add_python_binding NAME SWIGFILE LINKLIBRARY)
		set_source_files_properties (${SWIGFILE} PROPERTIES CPLUSPLUS ON)
		set_source_files_properties (${SWIGFILE} PROPERTIES SWIG_FLAGS "-py3;-extranative")
		swig_add_library (
			swig-python-${NAME}
			LANGUAGE python
			SOURCES ${SWIGFILE}
			TYPE MODULE)
		swig_link_libraries (swig-python-${NAME} ${LINKLIBRARY} ${PYTHON_LIBRARIES})
		set_target_properties (_swig-python-${NAME} PROPERTIES OUTPUT_NAME _${NAME})
		set_target_properties (_swig-python-${NAME} PROPERTIES SKIP_BUILD_RPATH TRUE)
		set_target_properties (_swig-python-${NAME} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
		set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")

		if (BUILD_TESTING)
			# add tests
			add_subdirectory (tests_${NAME})
		endif ()
	endmacro ()

	# kdb module
	add_python_binding (kdb kdb.i elektra-kdb)
	add_custom_command (
		TARGET _swig-python-kdb
		POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/__init__.py" ${CMAKE_SWIG_OUTDIR}/)

	# tools module
	#
	# we require SWIG 4 because of https://github.com/swig/swig/commit/9cb90982ee98b437fd3fe9f1257b48b5eef9b335
	if (${SWIG_VERSION} VERSION_LESS "4.0.0")
		message (WARNING "Python bindings for elektra tools require SWIG 4 or later. Not building.")
	else (${SWIG_VERSION} VERSION_LESS "4.0.0")
		add_python_binding (tools tools.i elektratools)
	endif (${SWIG_VERSION} VERSION_LESS "4.0.0")

	# merge module
	add_python_binding (merge merge.i elektra-merge)

	# install package
	set (PYTHON_GET_MODULES_DIR_COMMAND
	     "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
	execute_process (
		COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
		OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
		OUTPUT_STRIP_TRAILING_WHITESPACE)

	install (
		DIRECTORY ${CMAKE_SWIG_OUTDIR}
		DESTINATION ${PYTHON_SITE_PACKAGES}
		COMPONENT python3-elektra
		FILES_MATCHING
		REGEX "\\.(py|so)$")

endif ()

if (POLICY CMP0078)
	cmake_policy (POP)
endif (POLICY CMP0078)
