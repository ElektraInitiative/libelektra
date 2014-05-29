##
# This file sets compiler flags and things related
# to compiler detection
##


#
# The mode (standard) to be used by the compiler
#
set (C_MODE "-std=gnu99")

if (COMPILE_CXX11_MODE)
	set (CXX_MODE "-std=c++0x")
else()
	set (CXX_MODE "-std=c++98")
endif()


#
# Extra handling/flags for specific compilers/OS
#
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
	#older clang did not support non-pod-varargs (will compile, but crash if used)
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-error=non-pod-varargs")
	#not supported by icc:
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations")
	message (STATUS "Clang detected")
endif()

if (CMAKE_COMPILER_IS_GNUCXX)
	#not supported by icc:
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-deprecated-declarations")
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
endif ()


#
# Common flags can be used by both C and C++
#
set (COMMON_FLAGS "${COMMON_FLAGS} -pedantic")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wall -Wextra")
set (COMMON_FLAGS "${COMMON_FLAGS} -Wno-overlength-strings")


#
# Merge all flags
#
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_MODE} ${EXTRA_FLAGS} ${COMMON_FLAGS} -Wsign-compare -Wfloat-equal -Wformat-security")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_MODE} ${EXTRA_FLAGS} ${COMMON_FLAGS} -Wshadow -Wno-missing-field-initializers")

message (STATUS "C flags are ${CMAKE_C_FLAGS}")
message (STATUS "CXX flags are ${CMAKE_CXX_FLAGS}")

