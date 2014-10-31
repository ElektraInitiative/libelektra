##
# This file sets compiler flags and things related
# to compiler detection
#
#
# make sure to update src/plugins/constants/constants.c
#
# if new flags are added

#
# The mode (standard) to be used by the compiler
#
set (C_STD "-std=gnu99")

if (ENABLE_CXX11)
	set (CXX_STD "-std=c++11")
else()
	set (CXX_STD "-std=c++98")
endif()


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
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-ignored-qualifiers")

	message (STATUS "Clang detected")
endif()

if (CMAKE_COMPILER_IS_GNUCXX)
	#not supported by icc:
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations")
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-ignored-qualifiers")

	#not supported by icc/clang:
	set (CXX_EXTRA_FLAGS "${CXX_EXTRA_FLAGS} -Wstrict-null-sentinel")

	if (ENABLE_CXX11)
		set (CXX_STD "-std=gnu++11")
	else()
		set (CXX_STD "-std=gnu++98")
	endif()

	message (STATUS "GCC detected")
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
#
set (COMMON_FLAGS "${COMMON_FLAGS} -pedantic -Wno-variadic-macros")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wall -Wextra")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wno-overlength-strings")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wsign-compare -Wfloat-equal")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wformat-security")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wshadow")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wcomments -Wtrigraphs -Wundef")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wuninitialized")

set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--unresolved-symbols=ignore-in-shared-libs")


if (ENABLE_COVERAGE)
	set (COMMON_FLAGS "${COMMON_FLAGS} -fprofile-arcs -ftest-coverage")
	set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fprofile-arcs -ftest-coverage -lgcov")
endif (ENABLE_COVERAGE)



#
# Merge all flags
#
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_STD} ${EXTRA_FLAGS} ${COMMON_FLAGS} -Wsign-compare -Wfloat-equal -Wformat-security")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_STD} ${EXTRA_FLAGS} ${CXX_EXTRA_FLAGS} ${COMMON_FLAGS} -Wno-missing-field-initializers")

message (STATUS "C flags are ${CMAKE_C_FLAGS}")
message (STATUS "CXX flags are ${CMAKE_CXX_FLAGS}")

