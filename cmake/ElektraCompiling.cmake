##
# This file sets compiler flags and things related
# to compiler detection
#
#
# make sure to update src/plugins/constants/constants.c
#
# if new flags are added

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)

#
# The mode (standard) to be used by the compiler
#
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

check_cxx_compiler_flag(${CXX_STD} HAS_CXX_STD)
if (NOT HAS_CXX_STD)
	message(WARNING "Your compiler does not know the flag: ${CXX_STD}")
	set (CXX_STD "")
endif (NOT HAS_CXX_STD)

#
# check if -Wl,--version-script linker option is supported
# TODO: darwin ld only supports -Wl,-exported_symbols_list
#       + the file syntax is different
#
set(__symbols_file "${CMAKE_CURRENT_BINARY_DIR}/test-symbols.map")
file(WRITE ${__symbols_file} "{ local: *; };\n")
set(CMAKE_REQUIRED_FLAGS "-Wl,--version-script=${__symbols_file}")
check_cxx_compiler_flag("" LD_ACCEPTS_VERSION_SCRIPT)
unset(CMAKE_REQUIRED_FLAGS)


#
# Extra handling/flags for specific compilers/OS
#
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
	#older clang did not support non-pod-varargs (will compile, but crash if used)
	#so simply avoid to use it
	#icc also crashes (but just warns, no error)
	#set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-error=non-pod-varargs")

	#not supported by icc:
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations")
	#set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-ignored-qualifiers")
	set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wold-style-cast")

	message (STATUS "Clang detected")

	if (ENABLE_DEBUG)
		set (EXTRA_FLAGS "${EXTRA_FLAGS} -fsanitize=undefined -fsanitize=integer")
		set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -lubsan")
	endif()
endif()

if (ENABLE_ASAN)
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
	set (ASAN_LIBRARY "-lasan") #this is needed for GIR to put asan in front
	if (CMAKE_COMPILER_IS_GNUCXX)
		set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fsanitize=address")
		# this is currently a workaround so gtests compile with ASAN
		find_package(Threads)
		set (COMMON_FLAGS "${COMMON_FLAGS} ${CMAKE_THREAD_LIBS_INIT}")
	endif ()
	set (DISABLE_LSAN "LSAN_OPTIONS=detect_leaks=0") #this is needed so ASAN is not used during GIR compilation
	message (STATUS "To use ASAN:\n"
		 "   ASAN_OPTIONS=symbolize=1 ASAN_SYMBOLIZER_PATH=$(shell which llvm-symbolizer) ./bin \n"
		 "   It could also happen that you need to preload ASAN library: \n"
		 "   e.g. LD_PRELOAD=/usr/lib/clang/3.8.0/lib/linux/libclang_rt.asan-x86_64.so ./bin \n"
	)
endif ()

if (CMAKE_COMPILER_IS_GNUCXX)
	execute_process(COMMAND ${CMAKE_C_COMPILER} -dumpversion OUTPUT_VARIABLE GCC_VERSION)
	if (WIN32)
		message (STATUS "mingw detected")
	else(WIN32)
		#not supported by icc:
		set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations")
		#set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-ignored-qualifiers")
		set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wold-style-cast")

		#not supported by icc/clang:
		set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wstrict-null-sentinel")

		# needed by gcc4.7 for C++11 chrono
		set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -D_GLIBCXX_USE_NANOSLEEP")

		message (STATUS "GCC detected")
	endif(WIN32)
endif (CMAKE_COMPILER_IS_GNUCXX)

if (WIN32)
	set (HAVE_WIN32 "1")
	message (STATUS "Win32 detected")
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
	#statically link in libimf.so libsvml.so libirng.so libintlc.so.5
	#and fix warning #10237: -lcilkrts linked in dynamically, # static library not available
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -static-intel -wd10237")
	message (STATUS "ICC detected")

	# cmake bug: cmake thinks Intel does not know isystem
	set(CMAKE_INCLUDE_SYSTEM_FLAG_CXX "-isystem ")
endif ()


#
# Common flags can be used by both C and C++
# and by all supported compilers (gcc, mingw, icc, clang)
#
set (COMMON_FLAGS "${COMMON_FLAGS} -Wno-long-long") # allow long long in C++ code
set (COMMON_FLAGS "${COMMON_FLAGS} -pedantic -Wno-variadic-macros")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wall -Wextra")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wno-overlength-strings")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wsign-compare -Wfloat-equal")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wformat-security")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wshadow")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wcomments -Wtrigraphs -Wundef")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wuninitialized -Winit-self")

# Not every compiler understands -Wmaybe-uninitialized
check_c_compiler_flag(-Wmaybe-uninitialized HAS_CFLAG_MAYBE_UNINITIALIZED)
if (HAS_CFLAG_MAYBE_UNINITIALIZED)
	set (COMMON_FLAGS "${COMMON_FLAGS} -Wmaybe-uninitialized")
endif (HAS_CFLAG_MAYBE_UNINITIALIZED)

#set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined")

if (ENABLE_COVERAGE)
	set (COMMON_FLAGS "${COMMON_FLAGS} -fprofile-arcs -ftest-coverage")
	set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fprofile-arcs -ftest-coverage -lgcov")
endif (ENABLE_COVERAGE)


set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wno-missing-field-initializers")
set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Woverloaded-virtual  -Wsign-promo")

#
# Merge all flags
#
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_STD} ${EXTRA_FLAGS} ${COMMON_FLAGS} -Wsign-compare -Wfloat-equal -Wformat-security")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_STD} ${EXTRA_FLAGS} ${CXX_EXTRA_FLAGS} ${COMMON_FLAGS}")

message (STATUS "C flags are ${CMAKE_C_FLAGS}")
message (STATUS "CXX flags are ${CMAKE_CXX_FLAGS}")

