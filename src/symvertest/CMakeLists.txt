cmake_minimum_required (VERSION 3.0)
project (symvertest)

add_library (symvertest SHARED test.c)
set_target_properties (symvertest PROPERTIES LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/versions.map")

add_library (basic SHARED basic.c)
set_target_properties (basic PROPERTIES LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/basic.map")
