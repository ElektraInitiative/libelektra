check_binding_included ("intercept_fs" IS_INCLUDED SUBDIRECTORY "intercept/fs")
if (IS_INCLUDED)
	add_subdirectory (fs)
endif ()

check_binding_included ("intercept_env" IS_INCLUDED SUBDIRECTORY "intercept/env")
if (IS_INCLUDED)
	add_subdirectory (env)
endif ()
