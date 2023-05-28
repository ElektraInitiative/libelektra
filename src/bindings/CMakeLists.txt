include (LibAddBinding)

check_binding_included ("cpp" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (cpp)
endif ()

check_binding_included ("jna" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (jna)
endif ()

check_binding_included ("rust" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (rust)
endif ()

check_binding_included ("xfconf" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (xfconf)
endif ()

check_binding_included ("go-elektra" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (go-elektra)
endif ()

add_subdirectory (swig)

check_binding_included ("glib" IS_INCLUDED)
check_binding_included ("gsettings" IS_GSETTINGS_INCLUDED)
if (IS_INCLUDED
    OR IS_PYTHON_INCLUDED
    OR IS_LUA_INCLUDED
    OR IS_GSETTINGS_INCLUDED)
	add_subdirectory (glib)
endif ()

if (IS_GSETTINGS_INCLUDED)
	add_subdirectory (gsettings)
endif ()

add_subdirectory (intercept)

add_subdirectory (io)

check_binding_included ("elixir" IS_INCLUDED)
if (IS_INCLUDED)
	add_subdirectory (erl_nif)
endif ()
