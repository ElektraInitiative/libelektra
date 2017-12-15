set (SWIG_COMPILE_FLAGS "-Wno-shadow -Wno-old-style-cast -Wno-unused-variable")

find_swig ()
if (NOT SWIG_FOUND)
	remove_binding (swig "neither swig2/3 found. Please install swig3.0 and set -DSWIG_EXECUTABLE=" REMOVE_NOT_NECESSARY)
	return ()
endif ()

try_add_binding ("swig_python2" IS_INCLUDED SUBDIRECTORY "python2")
if (IS_INCLUDED)
	add_subdirectory (python2)
endif ()

try_add_binding ("swig_python" IS_INCLUDED SUBDIRECTORY "python")
if (IS_INCLUDED)
	add_subdirectory (python)
else ()
	# rename swig_python3 to swig_python - TODO remove sometime in the future
	try_add_binding ("swig_python3" IS_INCLUDED SUBDIRECTORY "python" SILENT)
	if (IS_INCLUDED)
		message (STATUS "swig_python3 has been renamed to swig_python")
		remove_binding ("swig_python3" "silent")
		add_binding_manual ("swig_python")
		add_subdirectory (python)
	endif ()
endif ()



try_add_binding ("swig_lua" IS_INCLUDED SUBDIRECTORY "lua")
if (IS_INCLUDED)
	add_subdirectory (lua)
endif ()

try_add_binding ("swig_ruby" IS_INCLUDED SUBDIRECTORY "ruby")
if (IS_INCLUDED)
	add_subdirectory (ruby)
endif ()
