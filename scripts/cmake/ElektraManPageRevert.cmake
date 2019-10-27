file (GLOB_RECURSE MAN_PAGES "${MAN_PAGE_DIRECTORY}/*")

# Revert newly generated man pages, where only the date changed.
foreach (MAN_PAGE ${MAN_PAGES})
	execute_process (COMMAND "${GIT_COMMAND}"
				 diff
				 --numstat
				 "${MAN_PAGE}"
			 OUTPUT_VARIABLE GIT_OUTPUT)
	string (REGEX
		REPLACE "^([0-9]+).*"
			"\\1"
			GIT_ADDED_LINES
			"${GIT_OUTPUT}")

	if (GIT_ADDED_LINES STREQUAL 1)
		execute_process (COMMAND "${GIT_COMMAND}" diff ${MAN_PAGE} OUTPUT_VARIABLE GIT_DIFF)
		if (GIT_DIFF MATCHES "\n-.TH")
			execute_process (COMMAND "${GIT_COMMAND}" checkout ${MAN_PAGE})
		endif (GIT_DIFF MATCHES "\n-.TH")
	endif (GIT_ADDED_LINES STREQUAL 1)
endforeach (MAN_PAGE ${MAN_PAGES})
