if (DEPENDENCY_PHASE)
	include (LibAddTest)
	add_gtest (testmod_ccode
		   SOURCES ccode.cpp
			   coder.cpp
		   LINK_ELEKTRA elektra-kdb
				elektra-plugin
		   INCLUDE_DIRECTORIES "${CMAKE_CURRENT_BINARY_DIR}" # Required for `readme_ccode.c`
				       "${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
endif (DEPENDENCY_PHASE)

add_plugin (ccode CPP SOURCES ccode.hpp ccode.cpp coder.hpp coder.cpp TEST_README TEST_REQUIRED_PLUGINS tcl base64)
