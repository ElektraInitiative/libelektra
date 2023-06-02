include (LibAddMacros)

macro (do_example source)
	set (SOURCES ${HDR_FILES} ${source}.c)
	add_executable (${source} ${SOURCES})
	target_include_directories (${source} PRIVATE "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include")

	install (
		TARGETS ${source}
		DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
		COMPONENT elektra-bin)

	set_target_properties (${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_example)

file (GLOB TESTS *.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_example (${name})
endforeach (file ${TESTS})
