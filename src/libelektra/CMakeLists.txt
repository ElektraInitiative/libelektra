file(GLOB src_files *.c ../include/*.h)

add_library(elektra ${src_files})
