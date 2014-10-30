list (FIND BINDINGS "swig_python2" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (python2)
endif ()

list (FIND BINDINGS "swig_python3" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (python3)
endif ()

list (FIND BINDINGS "swig_lua" FINDEX)
if (FINDEX GREATER -1)
	add_subdirectory (lua)
endif ()
