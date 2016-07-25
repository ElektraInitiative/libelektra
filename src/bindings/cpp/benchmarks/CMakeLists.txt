include(LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

macro (do_benchmark source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.cpp)
	add_executable (${source} ${SOURCES})
	add_dependencies (${source} kdberrors_generated)

	target_link_elektra(${source} elektra-kdb elektra-meta)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_benchmark)


file (GLOB TESTS *.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_benchmark (${name})
endforeach (file ${TESTS})

if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	# require by gethostname
	target_link_libraries (benchmark_thread "ws2_32")
	target_link_libraries (benchmark_sync "ws2_32")
endif ()
