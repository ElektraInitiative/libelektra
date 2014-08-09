include (LibAddMacros)

file (GLOB XML_FILES *.xml)
foreach (file ${XML_FILES})
	copy_file (${file} "${CMAKE_CURRENT_BINARY_DIR}")
endforeach (file ${XML_FILES})
