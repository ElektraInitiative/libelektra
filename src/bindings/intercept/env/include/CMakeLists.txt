file (GLOB HDR_FILES *.h)

install (
	FILES ${HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	COMPONENT libelektra-dev)
