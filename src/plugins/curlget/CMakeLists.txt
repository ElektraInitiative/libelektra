if (DEPENDENCY_PHASE)
	find_package(CURL)
	if(NOT CURL_FOUND)
		remove_plugin(curlget "Curl-dev not found")
	endif ()
	find_package(OpenSSL)
	if(NOT OPENSSL_FOUND)
		remove_plugin(curlget "OpenSSL-dev not found")
	endif ()
	if(OPENSSL_FOUND AND CURL_FOUND)
	    set (CURLGET_INCLUDE_DIRS ${CURL_INCLUDE_DIR} ${OPENSSL_INCLUDE_DIR})
	    set (CURLGET_LIBRARY_DIRS ${CURL_LIBRARIES} ${OPENSSL_LIBRARIES})
	endif ()
endif ()
		
add_plugin (curlget
	SOURCES
		curlget.h
		curlget.c
	INCLUDE_DIRECTORIES 
		${CURLGET_INCLUDE_DIRS}
	LINK_LIBRARIES
		${CURLGET_LIBRARY_DIRS}
	ADD_TEST
	)
