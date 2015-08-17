set (WRAP "-Wl")
set (WRAP "${WRAP},--wrap=getenv")
set (WRAP "${WRAP},--wrap=secure_getenv")
SET(CMAKE_REQUIRED_FLAGS ${WRAP})
CHECK_C_COMPILER_FLAG("" WRAP_GETENV)

# with --wrap=main, the example program of CHECK_C_COMPILER_FLAG would fail
set (WRAP "${WRAP},--wrap=main")

IF(WRAP_GETENV)
	add_subdirectory(include)
	add_subdirectory(src)
	add_subdirectory(examples)

	if (ENABLE_TESTING)
		if (BUILD_FULL OR BUILD_STATIC)
			add_subdirectory (tests)
		endif (BUILD_FULL OR BUILD_STATIC)
	endif(ENABLE_TESTING)
ELSE()
	message(STATUS "do not compile elektra-getenv because compiler flag ${WRAP} is not working")
ENDIF()
