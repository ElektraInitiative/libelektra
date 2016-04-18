cmake_minimum_required(VERSION 2.6.2)

include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${CMAKE_CURRENT_SOURCE_DIR}
	)

# needed by TEST_P
add_definitions(-DGTEST_HAS_PTHREAD=1)

########################################################################
#
# Defines the gtest & gtest_main libraries.  User tests should link
# with one of them.
add_library(gtest STATIC src/gtest-all.cc)
add_library(gtest_main STATIC src/gtest_main.cc)
target_link_libraries(gtest_main gtest)

