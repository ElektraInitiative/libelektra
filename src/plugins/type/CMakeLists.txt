if (DEPENDENCY_PHASE)
	include (LibAddTest)
	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
	add_gtest (testmod_type
		MEMLEAK
		SOURCES types.cpp
		)
endif ()


add_plugin (type
	CPP
	SOURCES
		type.hpp type.cpp
		types.hpp types.cpp
		type_checker.hpp
	)
