if (DEPENDENCY_PHASE)
	find_package(CURL)

	if(NOT (CURL_FOUND))
		remove_plugin(curlget "Curl-dev not found")
	endif ()
endif ()
		
add_plugin (curlget
	SOURCES
		curlget.h
		curlget.c
	INCLUDE_DIRECTORIES 
		${CURL_INCLUDE_DIR}
	LINK_LIBRARIES
		${CURL_LIBRARIES}
	)

add_plugintest (curlget)
