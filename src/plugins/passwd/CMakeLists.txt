include (SafeCheckSymbolExists)
include (LibAddMacros)

set (plugin passwd)

if (DEPENDENCY_PHASE)
	add_definitions (-D_GNU_SOURCE -D_DARWIN_C_SOURCE)
	safe_check_symbol_exists (fgetpwent "stdio.h;sys/types.h;pwd.h" TEST_FGETPWENT)
	safe_check_symbol_exists (getline "stdio.h" TEST_GETLINE)
	safe_check_symbol_exists (putpwent "stdio.h;sys/types.h;pwd.h" TEST_PUTPWENT)

	if (TEST_FGETPWENT)
		add_definitions (-DUSE_FGETPWENT)
	elseif (TEST_GETLINE)
		add_definitions (-DUSE_FGETPWENT_LOCAL)
	else ()
		remove_plugin (${plugin} "could not find fgetpwent nor getline.")
	endif ()

	# no need to remove the plugin as our fallbacks have no deps
	if (TEST_PUTPWENT)
		add_definitions (-DUSE_PUTPWENT)
	endif (TEST_PUTPWENT)
endif ()

add_plugin (
	passwd
	SOURCES passwd.h passwd.c
	COMPILE_DEFINITIONS ${PASSWD_COMPILE_DEFS}
	ADD_TEST INSTALL_TEST_DATA COMPONENT libelektra${SO_VERSION}-experimental)
