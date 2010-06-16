##
# This file sets compiler flags and things related
# to OS detection
##

# Set default compile flags for GCC
if (CMAKE_COMPILER_IS_GNUCXX)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic -std=gnu++98 -Wall -Wextra -Wno-missing-field-initializers")
	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic -std=gnu99 -Wall -Wsign-compare -Wfloat-equal -Wformat-security")
	message (STATUS "GCC detected, compile flags: ${CMAKE_C_FLAGS}")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (WIN32)
	set (HAVE_WIN32 "1")
endif (WIN32)
