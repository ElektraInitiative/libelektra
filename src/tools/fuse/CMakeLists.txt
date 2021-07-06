# The tool requires libfuse during runtime, but this is not enfoced here.

# Check if python3 with required version is available
find_package (Python3 3.8 COMPONENTS Interpreter)
if (NOT Python3_Interpreter_FOUND)
	remove_tool (fuse "did not find python3 interpreter of version >= 3.8")
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

# Install the package (the .whl file) and its dependencies (which are fetched from PyPI) into a subdirectory of the global installation
# location (--target)
install (CODE "execute_process(COMMAND
		${Python3_EXECUTABLE} -m pip install --target \"${CMAKE_INSTALL_PREFIX}/fuse\" --no-cache-dir ./dist/elektra_fuse-1.0.0-py3-none-any.whl
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )")

# Configure the entrypoint script with the installation path of the just installed python modules.
configure_file (fuse.in fuse)

# Copy the configured entrypoint script "fuse" to the proper place where kdb can find it.
install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/fuse DESTINATION "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}")

# The tool can now be invoked by "kdb fuse".
