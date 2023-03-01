function (swig_checks BINDING_NAME SUBDIRECTORY)
	if (NOT SWIG_FOUND)
		exclude_binding ("${BINDING_NAME}" "neither swig2/3/4 found. Please install swig 2/3/4 and set -DSWIG_EXECUTABLE=")
		return ()
	endif ()

	if (ENABLE_ASAN)
		exclude_binding ("${BINDING_NAME}" "SWIG is not compatible with ENABLE_ASAN")
		return ()
	endif (ENABLE_ASAN)

	if (IS_INCLUDED)
		add_subdirectory (${SUBDIRECTORY})
	endif ()
endfunction (swig_checks)

set (SWIG_COMPILE_FLAGS "-Wno-error -Wno-shadow -Wno-old-style-cast -Wno-unused-variable -Wno-unused-but-set-variable")
find_swig ()

check_binding_included ("python" IS_INCLUDED SUBDIRECTORY "swig/python")
if (IS_INCLUDED)
	swig_checks ("python" python)
endif ()

check_binding_included ("lua" IS_INCLUDED SUBDIRECTORY "swig/lua")
if (IS_INCLUDED)
	swig_checks ("lua" lua)
endif ()

check_binding_included ("ruby" IS_INCLUDED SUBDIRECTORY "swig/ruby")
if (IS_INCLUDED)
	swig_checks ("ruby" ruby)
endif ()
