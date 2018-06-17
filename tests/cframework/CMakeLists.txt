include (CheckFunctionExists)
include (LibAddMacros)

check_function_exists (nftw HAVE_NFTW)
check_function_exists (mkdtemp HAVE_MKDTEMP)
check_function_exists (setenv HAVE_SETENV)

if (HAVE_MKDTEMP AND HAVE_SETENV)
	add_definitions (-D_POSIX_C_SOURCE=200809L)
else (HAVE_MKDTEMP AND HAVE_SETENV)

	# XCode 9.2 on travis can not find the functions, but seems to work anyway.
	# Hence we progress forward with just a warning and do not disable
	# anything.
	message (WARNING "mkdtemp and setenv could not be found")
endif (HAVE_MKDTEMP AND HAVE_SETENV)

if (HAVE_NFTW)
	add_definitions (-D_XOPEN_SOURCE=500 -DHAVE_NFTW)
endif (HAVE_NFTW)

set (SOURCES tests.c)
add_headers (SOURCES)
add_testheaders (SOURCES)

add_library (cframework OBJECT ${SOURCES})
add_dependencies (cframework kdberrors_generated)

set_target_properties (cframework PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
