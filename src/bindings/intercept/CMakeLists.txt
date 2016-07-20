include (LibAddMacros)
add_headers(HDR_FILES)
add_library(intercept SHARED "intercept.c")
install (TARGETS intercept DESTINATION lib${LIB_SUFFIX})
