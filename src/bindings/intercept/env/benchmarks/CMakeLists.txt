include (LibAddMacros)

set_source_files_properties (benchmark_getenv.cpp PROPERTIES COMPILE_FLAGS "-Wno-unused-result")

macro (do_benchmark source)
	set (SOURCES ${HDR_FILES} ${source}.cpp)
	add_executable (${source} ${SOURCES})
	target_include_directories (${source} PRIVATE "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include")

	target_link_elektra (${source})

	set_target_properties (${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_benchmark)

file (GLOB TESTS benchmark_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_benchmark (${name})
endforeach (file ${TESTS})

target_link_libraries (benchmark_getenv elektraintercept-env elektra-meta)
