# this file overrides platform specific information

# we're unable to set this in Toolchain/Toolchain-mingw32.cmake because
# global platform information gets included afterwards
if (MINGW AND DEFINED PLATFORM_REQUIRE_DL)
	set(CMAKE_DL_LIBS "dl")
endif ()
