include (LibAddMacros)

check_binding_included ("gi_python"
			IS_PYTHON_INCLUDED
			SUBDIRECTORY
			"gi/python")
check_binding_included ("gi_lua"
			IS_LUA_INCLUDED
			SUBDIRECTORY
			"gi/lua")
check_binding_was_added ("glib" IS_GLIB_INCLUDED)

if (NOT IS_GLIB_ADDED)
	if (IS_PYTHON_INCLUDED)
		exclude_binding (gi_python "because gi_python depends on glib")
	endif ()
	if (IS_LUA_INCLUDED)
		exclude_binding (gi_lua "because gi_lua depends on glib")
	endif ()
	return ()
endif (NOT IS_GLIB_ADDED)

if (IS_PYTHON_INCLUDED OR IS_LUA_INCLUDED)

	find_package (GObjectIntrospection 1.38)

	if (INTROSPECTION_FOUND)

		if (ENABLE_ASAN AND UNIX AND NOT APPLE)
			set (ASAN_LIBRARY "-lubsan") # this is needed for GIR to put asan in front
			set (DISABLE_LSAN "LSAN_OPTIONS=detect_leaks=0") # this is needed so ASAN is not used during GIR compilation
		endif (ENABLE_ASAN AND UNIX AND NOT APPLE)

		add_headers (NOT_USED)

		set (GIR_NS "GElektra")
		set (GIR_PREFIX "${GIR_NS}-${SO_VERSION}.0")
		set (GIR_FILE "${GIR_PREFIX}.gir")
		set (GIR_TYPELIB "${GIR_PREFIX}.typelib")

		get_filename_component (GELEKTRA_HEADER_DIR ${CMAKE_CURRENT_BINARY_DIR}/../glib REALPATH)
		get_filename_component (GELEKTRA_LIBRARY_DIR ${CMAKE_BINARY_DIR}/lib REALPATH)
		set (GI_TYPELIB_DIR ${CMAKE_CURRENT_BINARY_DIR})

		string (TOLOWER ${GIR_NS} GIR_SYMBOL_PREFIX)
		file (GLOB GIR_INPUT "../glib/*.[hc]")

		get_directory_property (incdirs INCLUDE_DIRECTORIES)
		string (REPLACE ";"
				";-I"
				GIR_INCDIRS
				"${incdirs}")

		add_custom_command (OUTPUT ${GIR_FILE}
				    COMMAND ${DISABLE_LSAN}
					    ${INTROSPECTION_SCANNER}
					    --warn-all
					    --warn-error
					    --namespace=${GIR_NS}
					    --symbol-prefix=${GIR_SYMBOL_PREFIX}
					    --nsversion=${SO_VERSION}.0
					    --no-libtool
					    --library-path=${GELEKTRA_LIBRARY_DIR}
					    ${ASAN_LIBRARY}
					    --library=${GELEKTRA_LIBRARY}
					    --include=GObject-2.0
					    -I${GIR_INCDIRS}
					    --output=${GIR_FILE}
					    ${GELEKTRA_HEADER_DIR}/*.h
					    ${GIR_INPUT}
				    DEPENDS ${GIR_INPUT} ${GELEKTRA_LIBRARY}
				    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

		add_custom_command (OUTPUT ${GIR_TYPELIB}
				    COMMAND ${INTROSPECTION_COMPILER} --output=${GIR_TYPELIB} ${GIR_FILE}
				    DEPENDS ${GIR_FILE} ${GELEKTRA_LIBRARY}
				    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

		add_custom_target (gir ALL DEPENDS ${GIR_FILE} ${GIR_TYPELIB})
		add_dependencies (gir ${GELEKTRA_LIBRARY})

		if (INSTALL_SYSTEM_FILES)
			install (FILES ${CMAKE_CURRENT_BINARY_DIR}/${GIR_FILE} DESTINATION ${INTROSPECTION_GIRDIR})
			install (FILES ${CMAKE_CURRENT_BINARY_DIR}/${GIR_TYPELIB} DESTINATION ${INTROSPECTION_TYPELIBDIR})
		endif ()

		if (IS_PYTHON_INCLUDED)
			add_subdirectory (python)
		endif ()

		if (IS_LUA_INCLUDED)
			add_subdirectory (lua)
		endif ()
	else ()
		if (IS_PYTHON_INCLUDED)
			exclude_binding (gi_python "g-ir-scanner/g-ir-compiler not found. Please install GObject Introspection")
		endif ()
		if (IS_LUA_INCLUDED)
			exclude_binding (gi_lua "g-ir-scanner/g-ir-compiler not found. Please install GObject Introspection")
		endif ()
	endif ()
endif ()
