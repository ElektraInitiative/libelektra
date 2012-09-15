##
# This file sets compiler flags and things related
# to OS detection
##

set (C_MODE "-std=gnu99 -pedantic")

if (COMPILE_CXX11_MODE)
	set (CXX_MODE "-std=c++0x -pedantic")
else()
	set (CXX_MODE "-std=c++98 -pedantic")
endif()

set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wall -Wextra")

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
	set (EXTRA_FLAGS "${EXTRA_FLAGS} -Wno-error=non-pod-varargs")
	message (STATUS "Clang detected")
endif()

if (CMAKE_COMPILER_IS_GNUCXX)
	message (STATUS "GCC detected")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_COMPILER_IS_GNUCXX)
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_MODE} ${EXTRA_FLAGS} -Wsign-compare -Wfloat-equal -Wformat-security")
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_MODE} ${EXTRA_FLAGS} -Wshadow -Wno-missing-field-initializers")

	message (STATUS "C flags are ${CMAKE_C_FLAGS})
	message (STATUS "CXX flags are ${CMAKE_CXX_FLAGS})
endif (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_COMPILER_IS_GNUCXX)

if (WIN32)
	set (HAVE_WIN32 "1")
	message (STATUS "Win32 detected")
endif (WIN32)
