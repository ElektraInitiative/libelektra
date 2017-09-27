if (DEPENDENCY_PHASE)
	find_package(Boost QUIET)
	if (NOT Boost_FOUND)
		remove_plugin (yamlcpp "boost was not found")
	endif (NOT Boost_FOUND)

	find_package(yaml-cpp QUIET 0.5)
	if (NOT YAML-CPP_FOUND)
		remove_plugin (yamlcpp "yaml-cpp (version 0.5) was not found")
	endif (NOT YAML-CPP_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (yamlcpp
	CPP
	ADD_TEST
	INSTALL_TEST_DATA
	SOURCES
		yamlcpp.hpp yamlcpp.cpp
		read.hpp read.cpp
		write.hpp write.cpp
	INCLUDE_DIRECTORIES
		${YAML-CPP_INCLUDE_DIR} ${Boost_INCLUDE_DIR}
	LINK_LIBRARIES
		${YAML-CPP_LIBRARIES}
	LINK_ELEKTRA
		elektra-ease
	)
