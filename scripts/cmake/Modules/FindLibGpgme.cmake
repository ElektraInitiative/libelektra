# ~~~
# Detect the "GnuPG Made Easy" (GPGME) library on the system.
#
# LIBGPGME_FOUND - set if GPGME is detected
# LIBGPGME_CFLAGS - GPGME compiler flags
# LIBGPGME_LIBRARIES - GPGME linker flags
#
# Copyright (c) 2018 Peter Nirschl.
#
# Redistribution and use is allowed according to the terms of the BSD license. For details see the accompanying COPYING-CMAKE-SCRIPTS ile.
# ~~~

find_program (GPGME_EXECUTABLE NAMES gpgme-config)

# reset variables
set (LIBGPGME_LIBRARIES)
set (LIBGPGME_CFLAGS)

# if gpgme-config has been found
if (GPGME_EXECUTABLE)

	# workaround for MinGW/MSYS CMake can't starts shell scripts on windows so it needs to use sh.exe
	execute_process (
		COMMAND sh ${GPGME_EXECUTABLE} --libs
		RESULT_VARIABLE _return_VALUE
		OUTPUT_VARIABLE LIBGPGME_LIBRARIES
		OUTPUT_STRIP_TRAILING_WHITESPACE)
	execute_process (
		COMMAND sh ${GPGME_EXECUTABLE} --cflags
		RESULT_VARIABLE _return_VALUE
		OUTPUT_VARIABLE LIBGPGME_CFLAGS
		OUTPUT_STRIP_TRAILING_WHITESPACE)
	execute_process (
		COMMAND sh ${GPGME_EXECUTABLE} --version
		RESULT_VARIABLE _return_VALUE
		OUTPUT_VARIABLE LIBGPGME_VERSION
		OUTPUT_STRIP_TRAILING_WHITESPACE)

	if (NOT LIBGPGME_CFLAGS AND NOT _return_VALUE)
		set (LIBGPGME_CFLAGS " ")
	endif (NOT LIBGPGME_CFLAGS AND NOT _return_VALUE)

	if (LIBGPGME_LIBRARIES AND LIBGPGME_CFLAGS)
		set (LIBGPGME_FOUND TRUE)
	endif (LIBGPGME_LIBRARIES AND LIBGPGME_CFLAGS)

endif (GPGME_EXECUTABLE)

if (LIBGPGME_FOUND)

	if (NOT LibGpgme_FIND_QUIETLY)
		message (STATUS "Found GPGME: ${LIBGPGME_LIBRARIES}")
	endif (NOT LibGpgme_FIND_QUIETLY)

	# parse include directory from C-Flags
	string (LENGTH "${LIBGPGME_CFLAGS}" LIBGPGME_CFLAGS_LEN)
	if (${LIBGPGME_CFLAGS_LEN} GREATER 1)
		string (REPLACE "-I" "" LIBGPGME_INCLUDE_DIRS "${LIBGPGME_CFLAGS}")
		string (REPLACE " " ";" LIBGPGME_INCLUDE_DIRS "${LIBGPGME_INCLUDE_DIRS}")

	endif ()
	unset (LIBGPGME_CFLAGS_LEN)

else (LIBGPGME_FOUND)

	if (LibGpgme_FIND_REQUIRED)
		message (FATAL_ERROR "Could not find GPGME libraries")
	endif (LibGpgme_FIND_REQUIRED)

endif (LIBGPGME_FOUND)

mark_as_advanced (LIBGPGME_CFLAGS LIBGPGME_LIBRARIES)
