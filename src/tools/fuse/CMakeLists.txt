# The tool requires libfuse during runtime, but this is not enfoced here.

# Check if python binding is included
if (NOT "python" IN_LIST ADDED_BINDINGS)
	remove_tool (fuse "python binding not available")
	return ()
endif ()

# Check if python3 with required version is available
find_package (Python3 3.6 COMPONENTS Interpreter)
if (NOT Python3_Interpreter_FOUND)
	remove_tool (fuse "did not find python3 interpreter of version >= 3.6")
	return ()
endif ()

# Sometimes CMake returns an invalid version of python: check for that
execute_process (
	COMMAND ${Python3_EXECUTABLE} -c "import sys; exit(sys.version_info < (3, 6))"
	RESULT_VARIABLE EXIT_CODE
	OUTPUT_QUIET)

if (NOT EXIT_CODE EQUAL 0)
	remove_tool (fuse "python3 delivered by cmake not of version >= 3.6")
	return ()
endif ()

# Check if both pip and wheel are available
execute_process (
	COMMAND ${Python3_EXECUTABLE} -c "import pip,wheel"
	RESULT_VARIABLE EXIT_CODE
	OUTPUT_QUIET)

if (NOT EXIT_CODE EQUAL 0)
	remove_tool (fuse "python3-modules pip and wheel not both available")
	return ()
endif ()

# Create a temporary directory to build the python package
execute_process (
	COMMAND mktemp -d
	OUTPUT_VARIABLE TMP_BUILD_DIR
	OUTPUT_STRIP_TRAILING_WHITESPACE)

# Copy the source to the temporary directory
execute_process (COMMAND cp -a ${CMAKE_CURRENT_SOURCE_DIR}/. ${TMP_BUILD_DIR} OUTPUT_QUIET)

# Build the python wheel-package
execute_process (COMMAND ${Python3_EXECUTABLE} setup.py bdist_wheel WORKING_DIRECTORY ${TMP_BUILD_DIR})

# Check if running on bionic where https://github.com/pypa/pip/issues/3826 occurs. In that case, use additional flag.
execute_process (
	COMMAND sh -c "lsb_release -c | grep -E 'bionic'"
	RESULT_VARIABLE EXIT_CODE
	OUTPUT_QUIET)

if (EXIT_CODE EQUAL 0)
	set (PIP_BUG_WORKAROUND_OPTION "--system")
else ()
	set (PIP_BUG_WORKAROUND_OPTION "")
endif ()

# Install the package (the .whl file) and its dependencies (which are fetched from PyPI) to the local subdirectory 'fuse'
execute_process (COMMAND ${Python3_EXECUTABLE} -m pip install ${PIP_BUG_WORKAROUND_OPTION} --target \"${TMP_BUILD_DIR}/fuse\" --upgrade
			 --no-cache-dir ./dist/elektra_fuse-1.0.0-py3-none-any.whl WORKING_DIRECTORY ${TMP_BUILD_DIR})

# Copy the just created 'fuse' directory to the global installation directory. (This indirection is performed so that CPack knows about the
# directory.)
install (
	DIRECTORY ${TMP_BUILD_DIR}/fuse
	DESTINATION "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_DATA_FOLDER}"
	COMPONENT libelektra${SO_VERSION}-fuse)

# Configure the entrypoint script with the installation path of the just installed python modules.
configure_file (fuse.in ${TMP_BUILD_DIR}/fuse_entrypoint)

# Copy the configured entrypoint script "fuse_entrypoint" to the proper place where kdb can find it.
install (
	PROGRAMS ${TMP_BUILD_DIR}/fuse_entrypoint
	DESTINATION "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}/fuse"
	COMPONENT libelektra${SO_VERSION}-fuse)

# The tool can now be invoked by "kdb fuse".
