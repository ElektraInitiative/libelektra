# ~~~
# Try to find the Botan library.
#
# Defines:
#
#  Botan_FOUND - set if the system has the Botan library
#  Botan_LIBRARIES - the linker libraries needed to use the Botan library
#  Botan_INCLUDE_DIRS - the path to the include files of the Botan library
#
# Copyright (c) 2017 Peter Nirschl <peter.nirschl@gmail.com>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
# ~~~

find_package (PkgConfig QUIET)
if (PKG_CONFIG_FOUND)
	pkg_search_module (
		PC_Botan
		QUIET
		botan-1.10
		botan-1.9
		botan-1.8
		botan-2
		botan)
endif (PKG_CONFIG_FOUND)

find_path (
	Botan_INCLUDE_DIR
	NAMES botan/botan.h
	HINTS ${PC_Botan_INCLUDEDIR} ${PC_Botan_INCLUDE_DIRS}
	PATH_SUFFIXES botan-2 botan)

find_library (
	Botan_LIBRARY
	NAMES botan-1.10 botan-1.9 botan-1.8 botan-2 botan
	HINTS ${PC_Botan_LIBDIR} ${PC_Botan_LIBRARY_DIRS})

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Botan REQUIRED_VARS Botan_LIBRARY Botan_INCLUDE_DIR)

mark_as_advanced (Botan_INCLUDE_DIR Botan_LIBRARY)

set (Botan_LIBRARIES ${Botan_LIBRARY})
set (Botan_INCLUDE_DIRS ${Botan_INCLUDE_DIR})

if (Botan_FOUND)
	# try to compile and link a minimal sample program against libbotan
	try_compile (HAS_BOTAN_4SURE "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_botan.cpp"
		     CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${Botan_INCLUDE_DIRS} -DLINK_LIBRARIES:PATH=${Botan_LIBRARIES})

	if (NOT HAS_BOTAN_4SURE)
		message (STATUS "Botan compile/linker test failed")
		set (Botan_FOUND OFF)
		set (Botan_INCLUDE_DIRS "")
		set (Botan_LIBRARIES "")
	endif ()
endif (Botan_FOUND)
