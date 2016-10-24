find_package (Threads)

include (LibAddMacros)
include (LibAddTest)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)

file (GLOB rest-backend_SRCS ../*.cpp)
file (GLOB TESTS test_*.cpp)
foreach (file ${TESTS})
    get_filename_component (name ${file} NAME_WE)
    add_gtest (${name} LINK_TOOLS NO_MAIN SOURCES $<TARGET_OBJECTS:tmp_${tool}>)
    target_link_elektra (${name} ${Boost_LIBRARIES})
    target_link_elektra (${name} ${CPPCMS_LIBRARY})
    target_link_elektra (${name} ${LIBJWT_LIBRARY})
endforeach (file ${TESTS})
