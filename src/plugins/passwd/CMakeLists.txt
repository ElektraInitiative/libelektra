include (CheckFunctionExists)
include (LibAddMacros)

set (plugin passwd)

if (DEPENDENCY_PHASE)
	check_function_exists (fgetpwent TEST_FGETPWENT)
	check_function_exists (getline TEST_GETLINE)
	check_function_exists (putpwent TEST_PUTPWENT)

	if (TEST_FGETPWENT AND ALLOW_GNU_SOURCE)
		add_definitions (-D_GNU_SOURCE -DUSE_FGETPWENT)
	elseif (TEST_GETLINE)
		add_definitions (-D_POSIX_C_SOURCE=200809L -DUSE_GETLINE)
	else ()
		remove_plugin (${plugin} "could not find required functions: fgetpwent || getline")
	endif ()

	# no need to remove the plugin as our fallback has no deps
	if (TEST_PUTPWENT AND ALLOW_GNU_SOURCE)
		add_definitions (-D_GNU_SOURCE -DUSE_PUTPWENT)
	endif (TEST_PUTPWENT AND ALLOW_GNU_SOURCE)
endif ()

add_plugin (passwd
	    SOURCES passwd.h
		    passwd.c
	    COMPILE_DEFINITIONS ${PASSWD_COMPILE_DEFS}
	    ADD_TEST
	    INSTALL_TEST_DATA)
