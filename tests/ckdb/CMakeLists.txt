file (GLOB TESTS testckdb_*.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
	set_property(TEST ${name} PROPERTY LABELS kdbtests)
endforeach (file ${TESTS})

