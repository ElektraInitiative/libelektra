# ~~~
#
# This file sets compiler flags and things related
# to compiler detection
#
#
# make sure to update src/plugins/constants/constants.c
#
# if new flags are added
# ~~~

include (CheckCCompilerFlag)
include (CheckCXXCompilerFlag)

#
# The mode (standard) to be used by the compiler -
if (C_STD)
	message (STATUS "use C_STD as given by user: ${C_STD}")
else ()
	set (C_STD "-std=gnu99")
endif ()

if (CXX_STD)
	message (STATUS "use CXX_STD as given by user: ${CXX_STD}")
else ()
	set (CXX_STD "-std=c++11")
endif ()

check_cxx_compiler_flag (${CXX_STD} HAS_CXX_STD)
if (NOT HAS_CXX_STD)
	message (WARNING "Your compiler does not know the flag: ${CXX_STD}")
	set (CXX_STD "")
endif (NOT HAS_CXX_STD)

#
# check if -Wl,--version-script linker option is supported TODO: darwin ld only supports -Wl,-exported_symbols_list + the file syntax is
# different
#
try_compile (ELEKTRA_SYMVER_SUPPORTED ${CMAKE_BINARY_DIR}/src/symvertest/build ${CMAKE_SOURCE_DIR}/src/symvertest symvertest)
if (ELEKTRA_SYMVER_SUPPORTED)
	set (ELEKTRA_SYMVER_COMMAND "__asm__(\".symver \" arg1 \", \" arg2);")
	set (LD_ACCEPTS_VERSION_SCRIPT TRUE)
else (ELEKTRA_SYMVER_SUPPORTED)
	set (ELEKTRA_SYMVER_COMMAND "")

	try_compile (ELEKTRA_VERSION_SCRIPT_SUPPORTED ${CMAKE_BINARY_DIR}/src/symvertest/build ${CMAKE_SOURCE_DIR}/src/symvertest
											       symvertest basic)
	if (ELEKTRA_VERSION_SCRIPT_SUPPORTED)
		set (LD_ACCEPTS_VERSION_SCRIPT TRUE)
	else (ELEKTRA_VERSION_SCRIPT_SUPPORTED)
		set (LD_ACCEPTS_VERSION_SCRIPT FALSE)
	endif (ELEKTRA_VERSION_SCRIPT_SUPPORTED)
endif (ELEKTRA_SYMVER_SUPPORTED)

message (STATUS "compiler/linker accepts version script? ${LD_ACCEPTS_VERSION_SCRIPT}")
message (STATUS "compiler/linker supports symbol versioning? ${ELEKTRA_SYMVER_SUPPORTED}")

#
# Extra handling/flags for specific compilers/OS
#
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")

	# ~~~
	# older clang did not support non-pod-varargs (will compile, but crash if used)
	# so simply avoid to use it
	# icc also crashes (but just warns, no error)
	# ~~~
	# set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-error=non-pod-varargs")

	# not supported by icc:
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations") # set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-ignored-qualifiers")
	set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wold-style-cast")

	message (STATUS "Clang detected")
endif ()

if (CMAKE_COMPILER_IS_GNUCXX)
	execute_process (COMMAND ${CMAKE_C_COMPILER} -dumpversion OUTPUT_VARIABLE GCC_VERSION)
	if (WIN32)
		message (STATUS "mingw detected")

		# mingw builds need wine to be installed
		find_program (WINE "wine")
		if (WINE)
			message (STATUS "wine detected")
		else (WINE)
			message (FATAL_ERROR "wine could not be found but is needed for mingw builds")
		endif (WINE)
	else (WIN32)

		# not supported by icc:
		set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations")

		# set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-ignored-qualifiers")
		set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wold-style-cast")

		# not supported by icc/clang:
		set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wstrict-null-sentinel")

		# needed by gcc4.7 for C++11 chrono
		set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -D_GLIBCXX_USE_NANOSLEEP")

		message (STATUS "GCC detected")
	endif (WIN32)
endif (CMAKE_COMPILER_IS_GNUCXX)

#
# Platform specific settings
#
if (WIN32)
	set (HAVE_WIN32 "1")
	message (STATUS "Win32 detected")
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")

	# ~~~
	# statically link in libimf.so libsvml.so libirng.so libintlc.so.5
	# and fix warning #10237: -lcilkrts linked in dynamically, # static library not available
	# ~~~
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -static-intel -wd10237")
	message (STATUS "ICC detected")

	# cmake bug: cmake thinks Intel does not know isystem
	set (CMAKE_INCLUDE_SYSTEM_FLAG_CXX "-isystem ")
endif ()

#
# ASAN
#
if (ENABLE_ASAN)
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -fsanitize=undefined -fsanitize=address -fno-omit-frame-pointer")

	if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
		set (EXTRA_FLAGS "${EXTRA_FLAGS} -fsanitize=integer")
		set (EXTRA_FLAGS "${EXTRA_FLAGS} -fsanitize-blacklist=\"${CMAKE_SOURCE_DIR}/tests/sanitizer.blacklist\"")

		# In case the ubsan library exists, link it otherwise some tests will fail due to missing symbols on Linux
		if (CMAKE_SYSTEM_NAME MATCHES Linux)
			set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lubsan -fsanitize=address")
		endif (CMAKE_SYSTEM_NAME MATCHES Linux)
	endif ()

	if (CMAKE_COMPILER_IS_GNUCXX)

		# Work around error “unrecognized option '--push-state'”
		set (EXTRA_FLAGS "${EXTRA_FLAGS} -fuse-ld=gold")

		find_package (Threads QUIET) # this is needed because of wrong pthread detection
		# https://gcc.gnu.org/bugzilla/show_bug.cgi?id=69443
		set (THREAD_LIBS_AS_NEEDED "-Wl,--as-needed ${CMAKE_THREAD_LIBS_INIT}")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${THREAD_LIBS_AS_NEEDED}")
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${THREAD_LIBS_AS_NEEDED}")
	endif ()
endif ()

#
# Common flags can be used by both C and C++ and by all supported compilers (gcc, mingw, icc, clang)
#
set (COMMON_FLAGS "${COMMON_FLAGS} -Wno-long-long") # allow long long in C++ code
set (COMMON_FLAGS "${COMMON_FLAGS} -Wpedantic -Wno-variadic-macros")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wall -Wextra")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wno-overlength-strings")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wsign-compare -Wfloat-equal")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wformat -Wformat-security")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wshadow")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wcomments -Wtrigraphs -Wundef")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wuninitialized -Winit-self")
set (C_EXTRA_FLAGS "${C_EXTRA_FLAGS} -Wstrict-prototypes")

# Not every compiler understands -Wmaybe-uninitialized
check_c_compiler_flag (-Wmaybe-uninitialized HAS_CFLAG_MAYBE_UNINITIALIZED)
if (HAS_CFLAG_MAYBE_UNINITIALIZED)
	set (COMMON_FLAGS "${COMMON_FLAGS} -Wmaybe-uninitialized")
endif (HAS_CFLAG_MAYBE_UNINITIALIZED)

# set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined")

if (ENABLE_COVERAGE)
	set (COMMON_FLAGS "${COMMON_FLAGS} -fprofile-arcs -ftest-coverage")
	set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fprofile-arcs -ftest-coverage")
	if (NOT APPLE)
		set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -lgcov")
	endif (NOT APPLE)
endif (ENABLE_COVERAGE)

set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wno-missing-field-initializers")
set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Woverloaded-virtual  -Wsign-promo")

#
# Merge all flags
#
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_STD} ${EXTRA_FLAGS} ${C_EXTRA_FLAGS} ${COMMON_FLAGS} -Wsign-compare -Wfloat-equal")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_STD} ${EXTRA_FLAGS} ${CXX_EXTRA_FLAGS} ${COMMON_FLAGS}")

message (STATUS "C flags are ${CMAKE_C_FLAGS}")
message (STATUS "CXX flags are ${CMAKE_CXX_FLAGS}")
