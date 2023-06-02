include (LibAddMacros)

include (LibAddTest)

file (GLOB TESTS test_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} LINK_LIBRARIES elektraintercept-env)
endforeach (file ${TESTS})

# TODO: Fix memleak (cf. #4830)
if (test_getenv)
	set_property (TEST "test_getenv" PROPERTY LABELS memleak)
endif ()
