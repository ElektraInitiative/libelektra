file (GLOB TESTS test_*.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
endforeach (file ${TESTS})

target_link_libraries(test_array elektra-ease)
target_link_libraries(test_backend elektra-plugin)

target_link_libraries(test_mount elektra-plugin)
target_link_libraries(test_plugin elektra-plugin)
target_link_libraries(test_mountsplit elektra-plugin)
target_link_libraries(test_split elektra-plugin)
target_link_libraries(test_splitget elektra-plugin)
target_link_libraries(test_splitset elektra-plugin)

target_link_libraries(test_meta elektra-meta)
target_link_libraries(test_meta elektra-proposal)
target_link_libraries(test_key elektra-meta)
target_link_libraries(test_key elektra-proposal)

target_link_libraries(test_operation elektra-meta)
target_link_libraries(test_operation elektra-proposal)

target_link_libraries(test_proposal elektra-proposal)
