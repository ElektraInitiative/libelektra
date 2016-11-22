function (add_s_test NAME FILE)
	add_test (
		testshell_${NAME}
		"${CMAKE_CURRENT_SOURCE_DIR}/markdown_shell_recorder.sh"
		"${FILE}"
		)

	set_property(TEST testshell_${NAME} PROPERTY LABELS memleak kdbtests)
endfunction ()

add_s_test (enum "${CMAKE_SOURCE_DIR}/src/plugins/enum/README.md")
add_s_test (blockresolver "${CMAKE_SOURCE_DIR}/src/plugins/blockresolver/README.md")
add_s_test (conditionals "${CMAKE_SOURCE_DIR}/src/plugins/conditionals/README.md")
add_s_test (hosts "${CMAKE_SOURCE_DIR}/src/plugins/hosts/README.md")
add_s_test (line "${CMAKE_SOURCE_DIR}/src/plugins/line/README.md")
add_s_test (mathcheck "${CMAKE_SOURCE_DIR}/src/plugins/mathcheck/README.md")
add_s_test (mozpref "${CMAKE_SOURCE_DIR}/src/plugins/mozpref/README.md")
add_s_test (tutorial_mount "${CMAKE_SOURCE_DIR}/doc/tutorials/mount.md")
