cmake_minimum_required (VERSION 3.5)
project (elektrasettings)

#add_definitions("-fsanitize=address")

find_package(PkgConfig REQUIRED)
pkg_check_modules(GLIB glib-2.0>=2.48.0 REQUIRED)
include_directories(${GLIB_INCLUDE_DIRS})
pkg_check_modules(GMODULE gmodule-2.0>=2.48 REQUIRED)
pkg_check_modules(GIO gio-2.0>=2.48 REQUIRED)
pkg_check_modules(GELEKTRA gelektra-4.0>=0.8.16 REQUIRED)
include_directories(${GELEKTRA_INCLUDE_DIRS})
add_library(elektrasettings SHARED elektrasettingsbackend.c)
target_link_libraries(elektrasettings ${GLIB_LIBRARIES} ${GMODULE_LIBRARIES} ${GIO_LIBRARIES} ${GELEKTRA_LIBRARIES})
install(TARGETS elektrasettings LIBRARY DESTINATION "/usr/lib/gio/modules/")

if (CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	if (NOT FORCE_IN_SOURCE_BUILD)
		message(FATAL_ERROR
				"In-source builds are not permitted.\n"
				"Make a separate folder for building:\n"
				"    mkdir build && cd build && cmake ..\n"
				"Before that, remove the files already created:\n"
				"    rm -rf CMakeCache.txt CMakeFiles\n"
				"If you really know what you are doing\n"
				"(will overwrite original files!) use:\n"
				"    cmake -DFORCE_IN_SOURCE_BUILD=ON\n"
			)
	endif (NOT FORCE_IN_SOURCE_BUILD)
endif(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
