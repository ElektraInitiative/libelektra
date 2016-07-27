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

  include_directories(${RUBY_VENDOR_ARCH_DIR})
  include_directories(${RUBY_INCLUDE_DIRS})

  # just for debugging
  message (STATUS "Ruby executable: ${RUBY_EXECUTABLE}")
  message (STATUS "Ruby vendor arch dir: ${RUBY_VENDOR_ARCH_DIR}")
  message (STATUS "Ruby include path: ${RUBY_INCLUDE_PATH}")

	set_source_files_properties (kdb.i PROPERTIES CPLUSPLUS ON)
  #set_source_files_properties (kdb.i PROPERTIES SWIG_FLAGS "-py3;-extranative")
	
  set (CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")
	set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SWIG_OUTDIR}")
	set (CMAKE_SWIG_FLAGS "-O")

	swig_add_module (swig-ruby ruby kdb.i)
  swig_link_libraries (swig-ruby elektra-core elektra-kdb)
  #set_target_properties (_swig-ruby PROPERTIES OUTPUT_NAME _kdb)
  
  set_source_files_properties (
          ${swig_generated_file_fullname} PROPERTIES 
          COMPILE_FLAGS "${SWIG_COMPILE_FLAGS} -DSWIG_TYPE_TABLE=kdb"
  )

  #set (PYTHON_GET_MODULES_DIR_COMMAND "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")
	#execute_process (
	#	COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
	#	OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
	#	OUTPUT_STRIP_TRAILING_WHITESPACE
	#)

	#install (
	#	FILES ${CMAKE_CURRENT_BINARY_DIR}/kdb.py
	#	DESTINATION ${PYTHON_SITE_PACKAGES}
	#)

	#install (
	#	TARGETS _swig-python
	#	LIBRARY DESTINATION ${PYTHON_SITE_PACKAGES}
	#)

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})
  #include_directories (${CMAKE_SOURCE_DIR}/src/swig/)

  #add_subdirectory (tests)
endif()
