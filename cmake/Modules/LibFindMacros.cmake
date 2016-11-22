if (CMAKE_VERSION LESS 3.4)
	macro (pkg_get_variable OUT_VARIABLE MODULE PKG_VARIABLE)
		execute_process (
			COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=${PKG_VARIABLE} ${MODULE}
			OUTPUT_VARIABLE ${OUT_VARIABLE}
			OUTPUT_STRIP_TRAILING_WHITESPACE
		)
	endmacro (pkg_get_variable)
endif ()
