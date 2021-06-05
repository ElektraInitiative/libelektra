set (OLD_MAN_PAGE_COPY "${MANPAGE}.old")

if (EXISTS "${MANPAGE}" AND DIFF_COMMAND)
	execute_process (COMMAND ${CMAKE_COMMAND} -E copy ${MANPAGE} ${OLD_MAN_PAGE_COPY})
endif (EXISTS "${MANPAGE}" AND DIFF_COMMAND)

execute_process (COMMAND ${CMAKE_COMMAND} -E env RUBYOPT=-Eutf-8:utf-8 LC_ALL=C.utf-8 ${RONN_COMMAND} -r --pipe ${MDFILE}
		 OUTPUT_FILE ${MANPAGE})

if (NOT EXISTS "${OLD_MAN_PAGE_COPY}")
	return ()
endif (NOT EXISTS "${OLD_MAN_PAGE_COPY}")

# We revert newly generated man pages, where only the date has changed.
execute_process (COMMAND ${DIFF_COMMAND} -e ${MANPAGE} ${OLD_MAN_PAGE_COPY} OUTPUT_VARIABLE DIFF_OUTPUT)
string (REGEX MATCH "^4c\n\\.TH [^\n]+\n\\.\n$" ONLY_DATE_CHANGED "${DIFF_OUTPUT}")

if (NOT "${ONLY_DATE_CHANGED}" STREQUAL "")
	execute_process (COMMAND ${CMAKE_COMMAND} -E copy ${OLD_MAN_PAGE_COPY} ${MANPAGE})
endif (NOT "${ONLY_DATE_CHANGED}" STREQUAL "")

file (REMOVE "${OLD_MAN_PAGE_COPY}")
