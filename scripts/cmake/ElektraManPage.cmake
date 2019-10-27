execute_process (COMMAND ${CMAKE_COMMAND}
			 -E
			 env
			 RUBYOPT=-Eutf-8:utf-8
			 LC_ALL=C.utf-8
			 ${RONN_COMMAND}
			 -r
			 --pipe
			 ${MDFILE}
		 OUTPUT_FILE ${OUTFILE})

# We revert newly generated man pages, where only the date changed.
if (GIT_COMMAND)
	execute_process (COMMAND "${GIT_COMMAND}"
				 diff
				 --numstat
				 "${OUTFILE}"
			 OUTPUT_VARIABLE GIT_OUTPUT)
	string (REGEX
		REPLACE "^([0-9]+).*"
			"\\1"
			GIT_ADDED_LINES
			"${GIT_OUTPUT}")
	if (GIT_ADDED_LINES STREQUAL 1)
		execute_process (COMMAND "${GIT_COMMAND}" diff ${OUTFILE} OUTPUT_VARIABLE GIT_DIFF)
		if (GIT_DIFF MATCHES "\n-.TH")
			execute_process (COMMAND "${GIT_COMMAND}" checkout ${OUTFILE})
		endif (GIT_DIFF MATCHES "\n-.TH")
	endif (GIT_ADDED_LINES STREQUAL 1)
endif (GIT_COMMAND)
