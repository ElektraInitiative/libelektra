if (DEPENDENCY_PHASE)
	find_package (yaml-cpp QUIET 0.6)
	if (NOT YAML-CPP_FOUND)
		remove_plugin (yamlcpp "yaml-cpp (libyaml-cpp-dev >= 0.6) not found")
	endif (NOT YAML-CPP_FOUND)

	set (DISABLE_PLUGIN_ASAN
	     ${ENABLE_ASAN}
	     AND
	     "${CMAKE_CXX_COMPILER_ID}"
	     MATCHES
	     "GNU"
	     AND
	     ${CMAKE_CXX_COMPILER_VERSION}
	     VERSION_LESS
	     7)

	if (${DISABLE_PLUGIN_ASAN})
		remove_plugin (yamlcpp "ASAN enabled GCC builds of the plugin report memory leaks")
	endif (${DISABLE_PLUGIN_ASAN})
endif (DEPENDENCY_PHASE)

add_plugin (yamlcpp
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    INSTALL_TEST_DATA
	    TEST_REQUIRED_PLUGINS base64
				  directoryvalue
	    SOURCES yamlcpp.hpp
		    yamlcpp.cpp
		    read.hpp
		    read.cpp
		    write.hpp
		    write.cpp
	    INCLUDE_SYSTEM_DIRECTORIES ${YAML-CPP_INCLUDE_DIRS}
	    LINK_LIBRARIES ${YAML-CPP_LIBRARIES}
	    LINK_ELEKTRA elektra-ease)
