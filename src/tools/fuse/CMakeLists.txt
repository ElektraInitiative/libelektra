find_package (Python3 COMPONENTS Interpreter)

# This requires pip3 for installation, and libfuse during runtime. Maybe this should be enforced here?

if (Python3_Interpreter_FOUND)
	# Build the python wheel-package
	install (CODE "execute_process(COMMAND
			${Python3_EXECUTABLE} setup.py bdist_wheel
			WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )")

	# Install the package (the .whl file) and its dependencies (which are fetched from PyPI) into a subdirectory of the global
	# installation location (--target)
	install (CODE "execute_process(COMMAND
			${Python3_EXECUTABLE} -m pip install --target \"${CMAKE_INSTALL_PREFIX}/fuse\" --no-cache-dir ./dist/elektra_fuse-1.0.0-py3-none-any.whl
			WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )")

	# Configure the entrypoint script with the installation path of the just installed python modules.
	configure_file (fuse.in fuse)

	# Copy the configured entrypoint script "fuse" to the proper place where kdb can find it.
	install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/fuse DESTINATION "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}")

	# The tool can now be invoked by "kdb fuse".
else ()
	remove_tool (fuse "Did not find python3 interpreter")
endif ()
