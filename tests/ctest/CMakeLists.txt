file (GLOB TESTS test_*.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	if (ENABLE_OPTIMIZATIONS OR NOT ${name} MATCHES "opmphm")
		if (NOT "${name}" MATCHES "test_hooks")
			do_test (${name})
			target_link_elektra (${name} elektra-kdb)
		endif ()
	endif (ENABLE_OPTIMIZATIONS OR NOT ${name} MATCHES "opmphm")
endforeach (file ${TESTS})

include_directories ("${CMAKE_SOURCE_DIR}/src/libs/elektra")

target_link_elektra (test_array elektra-ease)
target_link_elektra (test_conversion elektra-ease)
target_link_elektra (test_keyname elektra-ease)

target_link_elektra (test_plugin elektra-plugin)
target_link_elektra (test_backends elektra-plugin)

target_link_elektra (test_meta elektra-meta)
target_link_elektra (test_ks elektra-ease)
target_link_elektra (test_key elektra-meta)
target_link_elektra (test_key_cow elektra-meta)
target_link_elektra (test_keyCopy_memerror elektra-meta)
target_link_elektra (test_ease_keyname elektra-ease)

target_link_elektra (test_operation elektra-meta)

target_link_elektra (test_utility elektra-utility)

target_link_elektra (test_globbing elektra-globbing)

target_link_elektra (test_opts elektra-opts)

target_link_elektra (test_cmerge elektra-merge)

target_link_elektra (test_sha-256 elektra-ease)

target_link_elektra (test_changetracking elektra-kdb)

target_link_elektra (test_diff elektra-kdb)
target_link_elektra (test_diffprivate elektra-kdb)

target_link_elektra (test_error elektra-kdb)
target_link_elektra (test_record elektra-kdb elektra-record)

if (TARGET elektra-notification
    AND TARGET elektra-dbus
    AND TARGET elektra-internalnotification
    AND TARGET elektra-gopts)
	do_test (test_hooks)
	target_link_elektra (test_hooks elektra-plugin elektra-notification)
else ()
	message (STATUS "excluding test_hooks, because elektra-notification not found")
endif ()

target_link_elektra (test_merge elektra-merge)

# LibGit leaks memory
set_property (TEST test_cmerge PROPERTY LABELS memleak)

# test_record reads and writes from kdb some test stages don't allow that and exclude all with the label "kdbtests"
set_property (TEST test_record PROPERTY LABELS kdbtests)
