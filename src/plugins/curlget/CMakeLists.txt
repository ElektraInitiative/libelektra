if (DEPENDENCY_PHASE)
	find_package(CURL)
	if(NOT CURL_FOUND)
		remove_plugin(curlget "Curl-dev not found")
	endif ()
	find_package(OpenSSL)
endif ()
		
add_plugin (curlget
	SOURCES
		curlget.h
		curlget.c
	INCLUDE_DIRECTORIES 
		${CURL_INCLUDE_DIR}
		${OPENSSL_INCLUDE_DIR}
	LINK_LIBRARIES
		${CURL_LIBRARIES}
		${OPENSSL_LIBRARIES}
	ADD_TEST
	)
