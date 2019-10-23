file (GLOB TESTS test_*.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	if (ENABLE_OPTIMIZATIONS OR NOT ${name} MATCHES "opmphm")
		do_test (${name})
		target_link_elektra (${name} elektra-kdb)
	endif (ENABLE_OPTIMIZATIONS OR NOT ${name} MATCHES "opmphm")
endforeach (file ${TESTS})

include_directories ("${CMAKE_SOURCE_DIR}/src/libs/elektra")

target_link_elektra (test_array elektra-ease)
target_link_elektra (test_conversion elektra-ease)
target_link_elektra (test_backend elektra-plugin)
target_link_elektra (test_keyname elektra-ease)

target_link_elektra (test_mount elektra-plugin)
target_link_elektra (test_plugin elektra-plugin)
target_link_elektra (test_mountsplit elektra-plugin)
target_link_elektra (test_split elektra-plugin)
target_link_elektra (test_splitget elektra-plugin)
target_link_elektra (test_splitset elektra-plugin)

target_link_elektra (test_meta elektra-meta)
target_link_elektra (test_meta elektra-proposal)
target_link_elektra (test_ks elektra-ease)
target_link_elektra (test_key elektra-meta)
target_link_elektra (test_key elektra-proposal)

target_link_elektra (test_operation elektra-meta)
target_link_elektra (test_operation elektra-proposal)

target_link_elektra (test_proposal elektra-proposal)

target_link_elektra (test_utility elektra-utility)

target_link_elektra (test_globbing elektra-globbing)

target_link_elektra (test_opts elektra-opts)

target_link_elektra (test_cmerge elektra-merge)
