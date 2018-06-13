if (DEPENDENCY_PHASE)
	include (LibAddTest)
	add_gtest (testmod_type
		   MEMLEAK
		   SOURCES types.cpp
		   LINK_ELEKTRA elektra-kdb # for elektraPluginOpen elektraPluginClose
		   INCLUDE_DIRECTORIES "${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
endif ()

add_plugin (type CPP SOURCES type.hpp type.cpp types.hpp types.cpp type_checker.hpp TEST_README)
