##
# This file sets compiler flags and things related
# to OS detection
##

# Set default compile flags for GCC
if (CMAKE_COMPILER_IS_GNUCXX)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -std=c++98 -Wall -Wextra -Wno-missing-field-initializers -O2")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic -std=c99 -Wall -Wsign-compare -Wfloat-equal -Wformat-security -O2")
	message (STATUS "GCC detected, compile flags: ${CMAKE_C_FLAGS}")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (WIN32)
	set (HAVE_WIN32 "1")
endif (WIN32)
