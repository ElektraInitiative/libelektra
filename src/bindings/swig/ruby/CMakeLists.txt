include (${SWIG_USE_FILE})
include (LibAddMacros)

if (APPLE)
	set (CMAKE_FIND_FRAMEWORK "LAST") # try user environment first, then search for system libs  required on OSX under rvm

	# Prefer Ruby versions installed via Homebrew
	set (RUBY_DIRECTORIES "/usr/local/opt/ruby@2.6" "/usr/local/opt/ruby")
	foreach (directory ${RUBY_DIRECTORIES})
		if (EXISTS ${directory})
			list (APPEND CMAKE_PREFIX_PATH ${directory})
		endif (EXISTS ${directory})
	endforeach (directory ${RUBY_DIRECTORIES})
endif ()

find_package (Ruby QUIET)

if (NOT RUBY_FOUND)
	exclude_binding (ruby "ruby interpreter or ruby header files not found (package ruby-dev/ruby-devel installed?)")
elseif (SWIG_VERSION VERSION_LESS "4.0.2" AND (RUBY_VERSION VERSION_GREATER "2.7.0" OR RUBY_VERSION VERSION_EQUAL "2.7.0"))
	exclude_binding (ruby "SWIG <= 4.0.1 incompatible with Ruby >= 2.7")
elseif (SWIG_VERSION MATCHES "^([12]\\.)|(3\\.0\\.[01234567])$")
	exclude_binding (ruby "found SWIG version (${SWIG_VERSION}) is not suitable. SWIG version >= 3.0.8 required")
else ()
	if (NOT BUILD_SHARED)
		exclude_binding (ruby "it can only be built if `BUILD_SHARED` is enabled")
		return ()
	endif (NOT BUILD_SHARED)

	add_binding (ruby)

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)
	add_toolheaders (HDR_FILES)

	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/include" "${CMAKE_SOURCE_DIR}/src/libs/tools/include")

	# minimal replacement for Ruby's mkmf.create_makefile
	execute_process (COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['vendorarchdir']"
			 OUTPUT_VARIABLE RUBY_VENDOR_ARCH_DIR)
	execute_process (COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['vendordir']" OUTPUT_VARIABLE RUBY_VENDOR_DIR)

	execute_process (COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['sitearchdir']"
			 OUTPUT_VARIABLE RUBY_SITE_ARCH_DIR)
	execute_process (COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['sitedir']" OUTPUT_VARIABLE RUBY_SITE_DIR)

	include_directories (SYSTEM ${RUBY_VENDOR_ARCH_DIR})
	include_directories (SYSTEM ${RUBY_INCLUDE_DIRS})

	# ~~~
	# message (STATUS "Ruby executable: ${RUBY_EXECUTABLE}")
	# message (STATUS "Ruby library: ${RUBY_LIBRARY}")
	# message (STATUS "Ruby version: ${RUBY_VERSION}")
	# to print RUBY_* variables for debugging build run cmake with -D_RUBY_DEBUG_OUTPUT=1
	# ~~~

	set_source_files_properties (kdb.i kdbtools.i PROPERTIES CPLUSPLUS ON)

	set_source_files_properties (kdb.i PROPERTIES SWIG_FLAGS "-initname;_kdb")
	set_source_files_properties (kdbtools.i PROPERTIES SWIG_FLAGS "-initname;_kdbtools;-minherit")

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O;-autorename;-DSWIG_NO_EXPORT_ITERATOR_METHODS;-DSWIG_WITHOUT_OVERRIDE")

	# specify the SWIG_TYPE_TABLE to use (has to be in sync with the ruby plugin)
	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb")

	# disable certain compiler warnings for SWIG generated files
	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-unused-parameter")
	set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-sign-compare")
	if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-unused-but-set-variable")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-catch-value")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-class-memaccess")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-maybe-uninitialized")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-attributes")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-pedantic")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-conversion-null")
	elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-dynamic-class-memaccess")

		# ~~~
		# TODO: Reenable the following warning after we add a virtual destructor to `PluginDatabase`, and its subclasses.
		# See also:
		# - https://github.com/ElektraInitiative/libelektra/pull/1841
		# - https://github.com/ElektraInitiative/libelektra/commit/15d67328
		# ~~~
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-delete-non-virtual-dtor")

		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-deprecated-register")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-gnu-zero-variadic-macro-arguments")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-zero-length-array")
		set (SWIG_COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -fdeclspec")
	endif ()

	# add 'kdb' module
	if (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_module (swig-ruby ruby kdb.i)
	else (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_library (
			swig-ruby
			LANGUAGE ruby
			SOURCES kdb.i
			TYPE MODULE)
	endif (CMAKE_VERSION VERSION_LESS 3.8) # set the compiler settings for the generated file  (has to be done for each module
	# separately)
	set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")

	# add the 'kdbtools' module
	if (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_module (swig-ruby-tools ruby kdbtools.i)
	else (CMAKE_VERSION VERSION_LESS 3.8)
		swig_add_library (
			swig-ruby-tools
			LANGUAGE ruby
			SOURCES kdbtools.i
			TYPE MODULE)
	endif (CMAKE_VERSION VERSION_LESS 3.8)
	set_source_files_properties (${swig_generated_file_fullname} PROPERTIES COMPILE_FLAGS "${SWIG_COMPILE_FLAGS}")

	swig_link_libraries (swig-ruby ${RUBY_LIBRARY} elektra-core elektra-kdb)
	set_target_properties (swig-ruby PROPERTIES OUTPUT_NAME _kdb PREFIX "")

	swig_link_libraries (swig-ruby-tools ${RUBY_LIBRARY} elektra-core elektra-kdb elektratools)
	set_target_properties (swig-ruby-tools PROPERTIES OUTPUT_NAME _kdbtools PREFIX "")

	# ~~~
	# CMAKE_INSTALL_PREFIX dependent install location
	# if we hit one of the usual cases (/usr/local or /usr) install the lib
	# in Ruby default include path
	# ~~~
	if (${CMAKE_INSTALL_PREFIX} MATCHES "^/usr/local/?$") # should be under /usr/local/lib/...
		set (RUBY_LIB_INSTALL_DIR "${RUBY_SITE_ARCH_DIR}")
		set (RUBY_MODULE_INSTALL_DIR "${RUBY_SITE_DIR}")
	elseif (${CMAKE_INSTALL_PREFIX} MATCHES "^/usr/?$") # should be under /usr/lib/...
		set (RUBY_LIB_INSTALL_DIR "${RUBY_VENDOR_ARCH_DIR}")
		set (RUBY_MODULE_INSTALL_DIR "${RUBY_VENDOR_DIR}")
	else () # fallback, define a custom location
		set (RUBY_LIB_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/ruby/site_ruby")
		set (RUBY_MODULE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib/ruby/site_ruby")
	endif ()

	if (_RUBY_DEBUG_OUTPUT)
		message (STATUS "Ruby lib install path: ${RUBY_LIB_INSTALL_DIR}")
		message (STATUS "Ruby module install path: ${RUBY_MODULE_INSTALL_DIR}")
	endif ()

	install (TARGETS swig-ruby swig-ruby-tools LIBRARY DESTINATION ${RUBY_LIB_INSTALL_DIR} COMPONENT ruby-elektra)
	install (
		FILES ${CMAKE_CURRENT_SOURCE_DIR}/kdb.rb ${CMAKE_CURRENT_SOURCE_DIR}/kdbtools.rb
		DESTINATION ${RUBY_MODULE_INSTALL_DIR}
		COMPONENT ruby-elektra)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	if (BUILD_TESTING)
		add_subdirectory (tests)
	endif (BUILD_TESTING)
endif ()
