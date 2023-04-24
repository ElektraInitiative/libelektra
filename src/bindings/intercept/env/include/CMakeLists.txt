file (GLOB HDR_FILES *.h)

# TODO: move installed headers
install (
	FILES ${HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	COMPONENT libelektra-dev)
