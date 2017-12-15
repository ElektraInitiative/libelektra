include(LibAddBinding)

try_add_binding ("cpp" IS_INCLUDED)
if (IS_INCLUDED)
	message (STATUS "Include Binding cpp")
	add_subdirectory (cpp)
endif ()

try_add_binding ("jna" IS_INCLUDED)
if (IS_INCLUDED)
	message (STATUS "Include Binding jna")
	add_subdirectory (jna)
endif ()

try_add_binding ("haskell" IS_INCLUDED)
if (IS_INCLUDED)
	message (STATUS "Include Binding haskell")
	add_subdirectory (haskell)
endif ()

add_subdirectory (swig)

try_add_binding ("glib" IS_GLIB_INCLUDED)
try_add_binding ("gsettings" IS_GSETTINGS_INCLUDED)
if (IS_GLIB_INCLUDED OR IS_GSETTINGS_INCLUDED)
	set (GELEKTRA_LIBRARY gelektra-${SO_VERSION}.0)
	add_subdirectory (glib)

	add_subdirectory (gi)

	if (IS_GSETTINGS_INCLUDED)
		if (NOT IS_GLIB_INCLUDED)
			# glib required by gsettings; announce binding
			add_binding_manual (glib)
		endif ()
		message (STATUS "Include Binding gsettings")
		add_subdirectory (gsettings)
	endif ()
endif ()

add_subdirectory (intercept)

add_subdirectory (io)
