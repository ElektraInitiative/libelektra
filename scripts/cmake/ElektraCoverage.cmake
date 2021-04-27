#
# coverage integration
#

find_program (COVERAGE_LCOV lcov)
find_program (COVERAGE_AWK awk)
if (EXISTS ${COVERAGE_LCOV} AND EXISTS ${COVERAGE_AWK})

	# make all invocations of lcov and genhtml quiet
	set (COMMON_FLAGS "-q")

	# directory for html+info files
	set (COVERAGE_DIR "${CMAKE_BINARY_DIR}/coverage")
	file (MAKE_DIRECTORY "${COVERAGE_DIR}")
	get_filename_component (COVERAGE_PREFIX ${COVERAGE_PREFIX} ABSOLUTE)

	# script to remove source files not from Elektra
	set (COVERAGE_FILTER "${PROJECT_BINARY_DIR}/scripts/build/filter-coverage.awk")
	configure_file ("${PROJECT_SOURCE_DIR}/scripts/build/filter-coverage.awk.in" "${COVERAGE_FILTER}" @ONLY)

	add_custom_target (
		coverage-start
		COMMAND ${COVERAGE_LCOV} ${COMMON_FLAGS} --directory . --zerocounters
		COMMAND ${COVERAGE_LCOV} ${COMMON_FLAGS} --directory . --initial --capture --output-file ${COVERAGE_DIR}/coverage-base.info
		WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
		COMMENT "Start capturing data for coverage.")

	add_custom_target (
		coverage-stop
		COMMAND ${COVERAGE_LCOV} ${COMMON_FLAGS} --directory . --capture --output-file ${COVERAGE_DIR}/coverage-test.info
		COMMAND ${COVERAGE_LCOV} ${COMMON_FLAGS} --add-tracefile ${COVERAGE_DIR}/coverage-test.info --add-tracefile
			${COVERAGE_DIR}/coverage-base.info --output-file ${COVERAGE_DIR}/coverage.info
		COMMAND ${COVERAGE_AWK} -f "${COVERAGE_FILTER}" ${COVERAGE_DIR}/coverage.info > ${COVERAGE_DIR}/coverage-filtered.info
		WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
		COMMENT "Stop capturing data for coverage.")

	find_program (COVERAGE_GENHTML genhtml)
	if (EXISTS ${COVERAGE_GENHTML})
		add_custom_target (
			coverage-genhtml
			COMMAND ${COVERAGE_GENHTML} ${COMMON_FLAGS} ${COVERAGE_DIR}/coverage-filtered.info --prefix "${COVERAGE_PREFIX}"
				--output-directory ${COVERAGE_DIR}
			WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
			COMMENT "Create html statistics of data for coverage.")

		add_dependencies (coverage-genhtml coverage-stop)
	else ()
		message (WARNING "no genhtml (coverage-genhtml target not added)")
	endif ()
else ()
	message (WARNING "no lcov and/or awk (coverage targets not added)")
endif ()
