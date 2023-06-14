include (LibAddBinding)

if (DEPENDENCY_PHASE)
	if (APPLE)
		set (CMAKE_FIND_FRAMEWORK "LAST")

		# Prefer Ruby versions installed via Homebrew
		set (RUBY_DIRECTORIES "/usr/local/opt/ruby@2.6" "/usr/local/opt/ruby")
		foreach (directory ${RUBY_DIRECTORIES})
			if (EXISTS ${directory})
				list (APPEND CMAKE_PREFIX_PATH ${directory})
			endif (EXISTS ${directory})
		endforeach (directory ${RUBY_DIRECTORIES})
	endif ()
	find_package (Ruby QUIET)
	find_swig ()
	check_binding_included ("ruby" BINDING_WAS_ADDED SUBDIRECTORY "swig/ruby" SILENT)

	# ~~~
	# Disable on GCC 4 and earlier:
	# - https://build.libelektra.org/job/elektra-gcc47-all
	# - https://build.libelektra.org/job/elektra-gcc-configure-debian-wheezy
	# ~~~
	set (SUPPORTED_COMPILER (NOT ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU" AND ${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 5)))

	if (NOT RUBY_FOUND)
		remove_plugin (ruby "ruby not found")
	elseif (NOT ${SUPPORTED_COMPILER})
		remove_plugin (ruby "gcc version too old (gcc -dumpversion < 5.0)")
	elseif (NOT SWIG_FOUND)
		remove_plugin (ruby "swig not found")
	elseif (NOT BINDING_WAS_ADDED)
		remove_plugin (ruby "ruby binding is required")
	elseif (SWIG_VERSION VERSION_LESS "4.0.2" AND (RUBY_VERSION VERSION_GREATER "2.7.0" OR RUBY_VERSION VERSION_EQUAL "2.7.0"))
		remove_plugin (ruby "SWIG <= 4.0.1 incompatible with Ruby >= 2.7")
	elseif (
		RUBY_FOUND
		AND SWIG_FOUND
		AND BINDING_WAS_ADDED
		AND ${SUPPORTED_COMPILER})
		include (LibAddMacros)

		add_custom_command (OUTPUT runtime.h COMMAND ${SWIG_EXECUTABLE} -c++ -ruby -external-runtime runtime.h)

		# we call this SWIG_COMPILE_FLAGS because we have the same variable in our swig bindings
		set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable -Wno-unused-parameter")

		# Disable warnings produced by generated code
		if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-deprecated-register")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-macro-redefined")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-reserved-user-defined-literal")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-gnu-zero-variadic-macro-arguments")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -fdeclspec")
		elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-literal-suffix")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-attributes")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-pedantic")
			set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-format-overflow")
		endif ()

		set_source_files_properties ("ruby.cpp" PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")
	endif ()
endif ()

add_plugin (
	ruby CPP
	SOURCES ruby.hpp ruby.cpp ${CMAKE_CURRENT_BINARY_DIR}/runtime.h
	INCLUDE_SYSTEM_DIRECTORIES ${RUBY_INCLUDE_DIRS}
	LINK_LIBRARIES ${RUBY_LIBRARY}
	COMPILE_DEFINITIONS SWIG_TYPE_TABLE=kdb SWIG_RUNTIME=\"runtime.h\" COMPONENT libelektra${SO_VERSION}-ruby)

if (ADDTESTING_PHASE)
	# bindings are required for tests
	check_binding_was_added ("ruby" BINDING_WAS_ADDED)
	if (BUILD_TESTING AND BINDING_WAS_ADDED)
		add_plugintest (ruby MEMLEAK
				ENVIRONMENT "RUBYLIB=${CMAKE_BINARY_DIR}/src/bindings/swig/ruby:${CMAKE_SOURCE_DIR}/src/bindings/swig/ruby")

		if (INSTALL_TESTING)
			# TODO: make clear via repo structure that this happens
			install (DIRECTORY ruby/ruby_test_scripts DESTINATION ${TARGET_TEST_DATA_FOLDER})
		endif ()
	else ()
		message (WARNING "ruby bindings are required for testing, test deactivated")
	endif ()
endif ()
