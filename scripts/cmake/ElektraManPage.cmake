set (OLD_MAN_PAGE_COPY "${MANPAGE}.old")

if (EXISTS "${MANPAGE}" AND DIFF_COMMAND)
	execute_process (COMMAND ${CMAKE_COMMAND}
				 -E
				 copy
				 ${MANPAGE}
				 ${OLD_MAN_PAGE_COPY})
endif (EXISTS "${MANPAGE}" AND DIFF_COMMAND)

execute_process (COMMAND ${CMAKE_COMMAND}
			 -E
			 env
			 RUBYOPT=-Eutf-8:utf-8
			 LC_ALL=C.utf-8
			 ${RONN_COMMAND}
			 -r
			 --pipe
			 ${MDFILE}
		 OUTPUT_FILE ${MANPAGE})

if (NOT EXISTS "${OLD_MAN_PAGE_COPY}")
	return ()
endif (NOT EXISTS "${OLD_MAN_PAGE_COPY}")

# We revert newly generated man pages, where only the date has changed.
execute_process (COMMAND ${DIFF_COMMAND} ${MANPAGE} ${OLD_MAN_PAGE_COPY} OUTPUT_VARIABLE DIFF_OUTPUT)
string (FIND "${DIFF_OUTPUT}" ">" DIFF_FIRST_POSITION_ADDED)
string (FIND "${DIFF_OUTPUT}" ">" DIFF_LAST_POSITION_ADDED REVERSE)

if ("${DIFF_FIRST_POSITION_ADDED}" STREQUAL "${DIFF_LAST_POSITION_ADDED}" AND "${DIFF_OUTPUT}" MATCHES "> .TH")
	execute_process (COMMAND ${CMAKE_COMMAND}
				 -E
				 copy
				 ${OLD_MAN_PAGE_COPY}
				 ${MANPAGE})
endif ("${DIFF_FIRST_POSITION_ADDED}" STREQUAL "${DIFF_LAST_POSITION_ADDED}" AND "${DIFF_OUTPUT}" MATCHES "> .TH")

file (REMOVE "${OLD_MAN_PAGE_COPY}")
