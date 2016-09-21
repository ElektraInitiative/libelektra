include (${SWIG_USE_FILE})
include(LibAddMacros)

find_package(Ruby)

if (NOT RUBY_FOUND)
  remove_binding(swig_ruby "ruby interpreter not found")
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
          COMMAND ${RUBY_EXECUTABLE} -r rbconfig -e "print RbConfig::CONFIG['sitearchdir']" 
          OUTPUT_VARIABLE RUBY_SITE_ARCH_DIR
  )

  include_directories(${RUBY_VENDOR_ARCH_DIR})
  include_directories(${RUBY_INCLUDE_DIRS})

  # just for debugging
  message (STATUS "Ruby executable: ${RUBY_EXECUTABLE}")
  message (STATUS "Ruby vendor arch dir: ${RUBY_VENDOR_ARCH_DIR}")
  message (STATUS "Ruby site arch dir: ${RUBY_SITE_ARCH_DIR}")
  message (STATUS "Ruby include path: ${RUBY_INCLUDE_PATH}")

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
	
  set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
  set (CMAKE_SWIG_FLAGS "-O;-autorename;-DSWIG_NO_EXPORT_ITERATOR_METHODS;-v")

  swig_add_module (swig-ruby ruby kdb.i)
  swig_link_libraries (swig-ruby elektra-core elektra-kdb)
  set_target_properties (swig-ruby PROPERTIES OUTPUT_NAME kdb)
  
  set_source_files_properties (
          ${swig_generated_file_fullname} PROPERTIES 
          COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -Wno-unused-parameter -Wno-unused-but-set-variable -DSWIG_TYPE_TABLE=kdb"
  )

  # for now just install in a fixed SITE_ARCH location (is under /usr/local)
  install (
          TARGETS swig-ruby
          LIBRARY DESTINATION ${RUBY_SITE_ARCH_DIR}
  )

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

  add_subdirectory (tests)
endif()
