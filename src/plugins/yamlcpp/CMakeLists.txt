include (LibAddMacros)

if (DEPENDENCY_PHASE)
	find_package(PkgConfig REQUIRED)
	pkg_check_modules(YAML_CPP QUIET yaml-cpp>=0.5)
	if (NOT YAML_CPP_FOUND)
		remove_plugin (yamlcpp "yaml-cpp (version 0.5 or newer) was not found")
	endif (NOT YAML_CPP_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (yamlcpp
	CPP
	ADD_TEST
	SOURCES
		yamlcpp.hpp
		yamlcpp.cpp
	INCLUDE_DIRECTORIES
		${YAML_CPP_INCLUDEDIR}/yaml-cpp
	LINK_LIBRARIES
		${YAML_CPP_LIBRARIES}
	)
