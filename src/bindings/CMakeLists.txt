include (LibAddBinding)

check_binding_included ("cpp" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (cpp)
endif ()

check_binding_included ("jna" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (jna)
endif ()

check_binding_included ("haskell" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (haskell)
endif ()

add_subdirectory (swig)

check_binding_included ("glib" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (glib)
endif ()

check_binding_included ("gsettings" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (gsettings)
endif ()

add_subdirectory (gi)

add_subdirectory (intercept)

add_subdirectory (io)
