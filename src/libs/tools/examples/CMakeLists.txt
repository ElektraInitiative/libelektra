include (LibAddMacros)

macro (do_example source)
	set (SOURCES ${HDR_FILES} ${source}.cpp)
	add_executable (${source} ${SOURCES})
	target_include_directories (${source} PRIVATE "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include"
						      "${CMAKE_CURRENT_SOURCE_DIR}/../include")

	target_link_elektratools (${source})

	set_target_properties (${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_example)

file (GLOB TESTS *.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_example (${name})
endforeach (file ${TESTS})
