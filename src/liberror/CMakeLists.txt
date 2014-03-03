add_executable(exporterrors exporterrors.cpp parser.hpp parser.cpp)

set_source_files_properties(exporterrors OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/specification)
