function (do_ruby_test source)
	cmake_parse_arguments (ARG "KDB_TEST" "" "REQUIRED_PLUGINS" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	get_filename_component (name ${source} NAME)
	add_test (NAME ${name} COMMAND ${RUBY_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/${source}" --verbose)

	# set RUBYLIB to find newly built binding lib
	set_property (TEST ${name} PROPERTY ENVIRONMENT "RUBYLIB=${CMAKE_CURRENT_BINARY_DIR}/..:${CMAKE_CURRENT_SOURCE_DIR}/.."
					    "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	set_property (TEST ${name} PROPERTY LABELS memleak bindings)

	if (ARG_KDB_TEST)
		set_property (
			TEST ${name}
			APPEND
			PROPERTY LABELS kdbtests)
		set_property (
			TEST ${name}
			APPEND
			PROPERTY RUN_SERIAL TRUE)
	endif ()
endfunction (do_ruby_test)

if (NOT (APPLE AND ENABLE_ASAN))
	do_ruby_test (testruby_key.rb)
	do_ruby_test (testruby_keyset.rb)
	do_ruby_test (testruby_kdb.rb KDB_TEST)
	do_ruby_test (testruby_gopts.rb KDB_TEST REQUIRED_PLUGINS gopts)

	do_ruby_test (testruby_tools_backendbuilder.rb)
	do_ruby_test (testruby_tools_backendparser.rb)
	do_ruby_test (testruby_tools_modules.rb)
	# do_ruby_test (testruby_tools_plugindatabase.rb REQUIRED_PLUGINS mini)
	do_ruby_test (testruby_tools_pluginspec.rb)
	do_ruby_test (testruby_tools_specreader.rb)
endif (NOT (APPLE AND ENABLE_ASAN))
