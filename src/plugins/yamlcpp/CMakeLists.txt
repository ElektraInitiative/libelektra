if (DEPENDENCY_PHASE)
	find_package (yaml-cpp QUIET 0.6)
	if (NOT YAML-CPP_FOUND)
		remove_plugin (yamlcpp "yaml-cpp (libyaml-cpp-dev >= 0.6) not found")
	endif (NOT YAML-CPP_FOUND)

	# AdressSanitizer enabled builds of the plugin report runtime errors about member calls, which do not point to an object of type
	# `_Sp_counted_base` inside the system header file `shared_ptr_base.h`. In Clang builds of the plugin we ignore this error in our
	# [blacklist](tests/sanitizer.blacklist). Unfortunately GCC does not support a blacklist, so we remove the plugin in this case.
	set (DISABLE_PLUGIN_ASAN
	     ${ENABLE_ASAN}
	     AND
	     "${CMAKE_CXX_COMPILER_ID}"
	     MATCHES
	     "GNU"
	     AND
	     ${CMAKE_CXX_COMPILER_VERSION}
	     VERSION_LESS
	     9)
	if (${DISABLE_PLUGIN_ASAN})
		remove_plugin (yamlcpp
			       "ASan enabled GCC builds of the plugin report member calls on addresses, "
			       "which do not point to an object of type `_Sp_counted_base`")
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
		    log.hpp
		    log.cpp
	    INCLUDE_SYSTEM_DIRECTORIES ${YAML-CPP_INCLUDE_DIRS}
	    LINK_LIBRARIES ${YAML-CPP_LIBRARIES}
	    LINK_ELEKTRA elektra-ease)
