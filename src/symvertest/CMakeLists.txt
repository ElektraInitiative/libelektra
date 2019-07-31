cmake_minimum_required (VERSION 3.0)

add_library (symvertest SHARED test.c)
set_target_properties (symvertest
		       PROPERTIES LINK_FLAGS
				  "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/versions.map")
