##
# This file sets compiler flags and things related
# to OS detection
##

if (COMPILE_CXX11_MODE)
	set (CXX_MODE "-std=c++0x")
else()
	set (CXX_MODE "-std=c++98")
endif()


if (CMAKE_CXX_COMPILER_ID MATCHES clang)
	set (CXX_EXTRA "${CXX_EXTRA} -Wno-error=non-pod-varargs")
endif()


# Set default compile flags for GCC
if (CMAKE_COMPILER_IS_GNUCXX)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_MODE} ${CXX_EXTRA} -Wshadow -Wall -Wextra -Wno-missing-field-initializers")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CXX_EXTRA} -pedantic -std=gnu99 -Wall -Wsign-compare -Wfloat-equal -Wformat-security")
	message (STATUS "GCC detected, compile flags: ${CMAKE_C_FLAGS}")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (WIN32)
	set (HAVE_WIN32 "1")
endif (WIN32)
