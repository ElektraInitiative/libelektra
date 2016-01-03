find_package(Threads)

add_plugin (type
	CPP
	SOURCES
		type.hpp type.cpp
		types.hpp types.cpp
		type_checker.hpp
	)

include (LibAddTest)
include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
add_gtest (testmod_type
		MEMLEAK
		TARGET_LINK_LIBRARIES ${CMAKE_THREAD_LIBS_INIT}
		SOURCES types.cpp
		)
