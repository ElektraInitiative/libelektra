include(LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

macro (do_benchmark source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.cpp)
	add_executable (${source} ${SOURCES})

	target_link_elektra(${source})

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_benchmark)


if (ENABLE_CXX11)
	file (GLOB TESTS cpp11*.cpp)
	foreach (file ${TESTS})
		get_filename_component (name ${file} NAME_WE)
		do_benchmark (${name})
	endforeach (file ${TESTS})

	if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
		# require by gethostname
		target_link_libraries (cpp11_benchmark_thread "ws2_32")
	endif ()
endif (ENABLE_CXX11)
