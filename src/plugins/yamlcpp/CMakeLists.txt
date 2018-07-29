if (DEPENDENCY_PHASE)
	find_package (yaml-cpp QUIET 0.5.2)
	if (NOT YAML-CPP_FOUND)
		# The MSR test for version 0.5.1 fails, because of a bug in yaml-cpp: https://github.com/jbeder/yaml-cpp/issues/247
		remove_plugin (yamlcpp "yaml-cpp (libyaml-cpp-dev >= 0.5.2) not found")
	else (NOT YAML-CPP_FOUND)
		if (YAML-CPP_VERSION VERSION_LESS "0.6.0")
			if (ENABLE_ASAN AND CMAKE_COMPILER_IS_GNUCXX)
				remove_plugin (yamlcpp
					       "yaml-cpp ${YAML-CPP_VERSION} reports runtime errors if you compile it with GCC "
					       "using AddressSanitizer")
			endif (ENABLE_ASAN AND CMAKE_COMPILER_IS_GNUCXX)

			find_package (Boost QUIET)
			if (NOT Boost_FOUND)
				remove_plugin (yamlcpp "yaml-cpp ${YAML-CPP_VERSION} requires Boost")
			endif (NOT Boost_FOUND)

			# Disable warnings about shadow declarations in yaml-cpp source code
			if (APPLE AND CMAKE_COMPILER_IS_GNUCXX)
				string (REPLACE "-Wshadow"
						""
						CMAKE_CXX_FLAGS
						"${CMAKE_CXX_FLAGS}")
			endif (APPLE AND CMAKE_COMPILER_IS_GNUCXX)
			set (YAML_PLUGIN_INCLUDE_DIRS ${Boost_INCLUDE_DIR})
		endif (YAML-CPP_VERSION VERSION_LESS "0.6.0")
	endif (NOT YAML-CPP_FOUND)

	set (YAML_PLUGIN_INCLUDE_DIRS ${YAML_PLUGIN_INCLUDE_DIRS} ${YAML-CPP_INCLUDE_DIR})
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
	    INCLUDE_DIRECTORIES ${YAML_PLUGIN_INCLUDE_DIRS}
	    LINK_LIBRARIES ${YAML-CPP_LIBRARIES}
	    LINK_ELEKTRA elektra-ease)
