if (DEPENDENCY_PHASE)
	find_package (yaml-cpp QUIET 0.6)
	if (NOT YAML-CPP_FOUND)
		remove_plugin (yamlcpp "yaml-cpp (libyaml-cpp-dev >= 0.6) not found")
	endif (NOT YAML-CPP_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (yamlcpp
	    CPP
	    ADD_TEST
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
