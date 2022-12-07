execute_process (COMMAND ${GIT_COMMAND} status --porcelain -- "${SOURCE_FILE}" OUTPUT_VARIABLE GITSTATUS)

if (NOT GITSTATUS)
	# no output means unmodified -> use date from git log
	execute_process (COMMAND ${GIT_COMMAND} log -1 --format=%ad --date=short -- "${SOURCE_FILE}" OUTPUT_VARIABLE DATE)
	string (STRIP ${DATE} DATE)
endif (NOT GITSTATUS)

# there is no date in the git log, or the file was modified -> use current date
if (NOT DATE)
	string (TIMESTAMP DATE "%Y-%m-%d")
endif (NOT DATE)

execute_process (
	COMMAND "${CMAKE_COMMAND}" -E env RUBYOPT=-Eutf-8:utf-8 LC_ALL=C.utf-8 "${RONN_COMMAND}" -r --pipe "${MDFILE}" "--date=${DATE}"
	OUTPUT_FILE ${MAN_PAGE}
	ERROR_VARIABLE ERROR_OUTPUT
	RESULT_VARIABLE RESULT)

if (NOT RESULT EQUAL 0)
	message (FATAL_ERROR "${RONN_COMMAND} -r --pipe ${MDFILE} --date=${DATE} failed with output: ${ERROR_OUTPUT}")
endif ()
