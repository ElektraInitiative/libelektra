# The tool requires libfuse during runtime, but this is not enfoced here.

# Check if python3 with required version is available
find_package (Python3 3.8 COMPONENTS Interpreter)
if (NOT Python3_Interpreter_FOUND)
	remove_tool (fuse "did not find python3 interpreter of version >= 3.8")
	return ()
endif ()

# Sometimes CMake returns an invalid version of python: check for that
execute_process (
	COMMAND ${Python3_EXECUTABLE} -c "import sys; exit(sys.version_info < (3, 8))"
	RESULT_VARIABLE EXIT_CODE
	OUTPUT_QUIET)

if (NOT EXIT_CODE EQUAL 0)
	remove_tool (fuse "python3 delivered by cmake not of version >= 3.8")
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

# Build the python wheel-package
install (CODE "execute_process(COMMAND
		${Python3_EXECUTABLE} setup.py bdist_wheel
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )")

# Install the package (the .whl file) and its dependencies (which are fetched from PyPI) to the local subdirectory 'fuse'
install (
	CODE "execute_process(COMMAND
		${Python3_EXECUTABLE} -m pip install --target \"${CMAKE_CURRENT_SOURCE_DIR}/fuse\" --upgrade --no-cache-dir ./dist/elektra_fuse-1.0.0-py3-none-any.whl
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )")

# Copy the just created 'fuse' directory to the global installation directory. (This indirection is performed so that CPack knows about the
# directory.)
install (
	DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/fuse
	DESTINATION "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_DATA_FOLDER}"
	COMPONENT libelektra${SO_VERSION}-fuse)

# Configure the entrypoint script with the installation path of the just installed python modules.
configure_file (fuse.in fuse)

# Copy the configured entrypoint script "fuse" to the proper place where kdb can find it.
install (
	PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/fuse
	DESTINATION "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}"
	COMPONENT libelektra${SO_VERSION}-fuse)

# The tool can now be invoked by "kdb fuse".
