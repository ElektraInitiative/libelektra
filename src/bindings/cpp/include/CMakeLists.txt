file (GLOB HEADER_FILES *.hpp)

# TODO: move installed headers
install (
	FILES ${HEADER_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	COMPONENT libelektra-dev)
