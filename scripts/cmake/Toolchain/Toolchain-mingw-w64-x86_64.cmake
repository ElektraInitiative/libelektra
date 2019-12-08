# the name of the target operating system
set (CMAKE_SYSTEM_NAME Windows)

# which compilers to use for C and C++
set (CMAKE_C_COMPILER x86_64-w64-mingw32-gcc-posix)
set (CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++-posix)
set (CMAKE_RC_COMPILER x86_64-w64-mingw32-windres)

# link libraries statically. wine is unable to find them
set (
	CMAKE_SHARED_LINKER_FLAGS
	"-static"
	CACHE STRING "" FORCE)
set (
	CMAKE_EXE_LINKER_FLAGS
	"-static"
	CACHE STRING "" FORCE)

# adjust the default behaviour of the FIND_XXX() commands: search headers and libraries in the target environment, search programs in the
# host environment
set (CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set (CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set (CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
