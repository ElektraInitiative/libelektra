include (${SWIG_USE_FILE})
include(LibAddMacros)

if (APPLE)
	# try user environment first, then search for system libs
	# required on OSX under rvm
	set (CMAKE_FIND_FRAMEWORK "LAST")
endif()

find_package(Ruby)

if (NOT RUBY_FOUND)
	remove_binding(swig_ruby "ruby interpreter or ruby header files not found (package ruby-dev/ruby-devel installed?)")
else()

	message(STATUS "Include Binding swig_ruby")

	add_headers (HDR_FILES)
	add_cppheaders (HDR_FILES)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

	# minimal replacement for Ruby's mkmf.create_makefile
	execute_process (
		COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['vendorarchdir']"
		OUTPUT_VARIABLE RUBY_VENDOR_ARCH_DIR
	)
	execute_process (
		COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['vendordir']"
		OUTPUT_VARIABLE RUBY_VENDOR_DIR
	)

	execute_process (
		COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['sitearchdir']"
		OUTPUT_VARIABLE RUBY_SITE_ARCH_DIR
	)
	execute_process (
		COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['sitedir']"
		OUTPUT_VARIABLE RUBY_SITE_DIR
	)

	include_directories(${RUBY_VENDOR_ARCH_DIR})
	include_directories(${RUBY_INCLUDE_DIRS})

	#message (STATUS "Ruby executable: ${RUBY_EXECUTABLE}")
	#message (STATUS "Ruby library: ${RUBY_LIBRARY}")
	#message (STATUS "Ruby version: ${RUBY_VERSION}")
	# to print RUBY_* variables for debugging build run cmake with -D_RUBY_DEBUG_OUTPUT=1

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)

	set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-initname;_kdb;-O;-autorename;-DSWIG_NO_EXPORT_ITERATOR_METHODS")

	swig_add_module (swig-ruby ruby kdb.i)
	swig_link_libraries (swig-ruby ${RUBY_LIBRARY} elektra-core elektra-kdb)
	set_target_properties (swig-ruby PROPERTIES
		OUTPUT_NAME _kdb
		PREFIX ""
	)

	set_source_files_properties (
		${swig_generated_file_fullname} PROPERTIES
		COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-unused-parameter -Wno-unused-but-set-variable -DSWIG_TYPE_TABLE=kdb"
	)


	# CMAKE_INSTALL_PREFIX dependent install location
	# if we hit one of the usual cases (/usr/local or /usr) install the lib
	# in Ruby default include path
	if (${CMAKE_INSTALL_PREFIX} MATCHES "^/usr/local/?$")
		# should be under /usr/local/lib/...
		set (RUBY_LIB_INSTALL_DIR "${RUBY_SITE_ARCH_DIR}")
		set (RUBY_MODULE_INSTALL_DIR "${RUBY_SITE_DIR}")
	elseif(${CMAKE_INSTALL_PREFIX} MATCHES "^/usr/?$")
		# should be under /usr/lib/...
		set (RUBY_LIB_INSTALL_DIR "${RUBY_VENDOR_ARCH_DIR}")
		set (RUBY_MODULE_INSTALL_DIR "${RUBY_VENDOR_DIR}")
	else()
		# fallback, define a custom location
		set (RUBY_LIB_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/ruby/site_ruby")
		set (RUBY_MODULE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib/ruby/site_ruby")
	endif()

	if(_RUBY_DEBUG_OUTPUT)
		message (STATUS "Ruby lib install path: ${RUBY_LIB_INSTALL_DIR}")
		message (STATUS "Ruby module install path: ${RUBY_MODULE_INSTALL_DIR}")
	endif()

	install (
		TARGETS swig-ruby
		LIBRARY DESTINATION ${RUBY_LIB_INSTALL_DIR}
	)
	install (
		FILES ${CMAKE_CURRENT_SOURCE_DIR}/kdb.rb
		DESTINATION ${RUBY_MODULE_INSTALL_DIR}
	)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
	if (BUILD_TESTING)
		add_subdirectory (tests)
	endif (BUILD_TESTING)
endif()
