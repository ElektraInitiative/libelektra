install (PROGRAMS elektra-merge DESTINATION bin)

IF (IS_DIRECTORY /etc/bash_completion.d)
	install (PROGRAMS kdb-bash-completion 
			DESTINATION /etc/bash_completion.d 
			RENAME kdb
			PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
ENDIF()