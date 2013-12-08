include (LibAddMacros)

file (GLOB SCRIPT_TESTS *.sh)
foreach (file ${SCRIPT_TESTS})
	get_filename_component (name ${file} NAME)
	add_scripttest(${name})
endforeach (file ${SCRIPT_TESTS})
