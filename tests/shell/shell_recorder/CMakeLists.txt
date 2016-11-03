if (ENABLE_KDB_TESTING)
        # set kdb command
        SET(USE_CMAKE_KDB_COMMAND "")
	if (BUILD_SHARED)
                set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/kdb")
        elseif (BUILD_FULL)
                set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/kdb-full")
        elseif (BUILD_STATIC)
                set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/kdb-static")
        else()
                message(SEND_ERROR "no kdb tool found, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
        endif ()
        configure_file (
                        "${CMAKE_CURRENT_SOURCE_DIR}/shell_recorder.sh"
                        "${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh"
                        @ONLY
                )

        file (GLOB SCRIPT_TESTS *.esr)
        foreach (file ${SCRIPT_TESTS})
		file (READ ${file} content)
		get_filename_component (name ${file} NAME)
                get_filename_component (testname_we ${name} NAME_WE)
                set (add 1)
		string (REGEX MATCH "Plugins:[ ]?[a-zA-Z0-9;]*" neededPlugins "${content}")
		string (LENGTH "${neededPlugins}" len)
		if (${len} GREATER 8)
			string (SUBSTRING "${neededPlugins}" 8 -1 neededPlugins)
			string (STRIP "${neededPlugins}" neededPlugins)
			foreach (p ${neededPlugins})
				list (FIND ADDED_PLUGINS ${p} ret)
				if (${ret} LESS 0)
					message (STATUS "Exclude ${name} from Shell Recorder Test")
					set (add 0)
				endif (${ret} LESS 0)
			endforeach (p)
		endif (${len} GREATER 8)
		if (${add})
                add_test (
                        testshell_${testname_we}
                        "${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh"
                        "${CMAKE_CURRENT_SOURCE_DIR}/${name}"
                        )

                set_property(TEST testshell_${testname_we} PROPERTY LABELS memleak kdbtests)
                endif (${add})
        endforeach (file ${SCRIPT_TESTS})

endif (ENABLE_KDB_TESTING)
