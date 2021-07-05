execute_process (COMMAND ${CMAKE_COMMAND} -E env RUBYOPT=-Eutf-8:utf-8 LC_ALL=C.utf-8 ${RONN_COMMAND} -r --pipe ${MDFILE} --date=${DATE}
		 OUTPUT_FILE ${MANPAGE})
