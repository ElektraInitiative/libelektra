set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_FLAGS "${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")

set(CMAKE_BUILD_TYPE Debug)

add_definitions(-Wall)

find_package(Qt5 COMPONENTS Core Quick Gui Qml Widgets Test)
if (NOT Qt5_FOUND)
	remove_tool(qt-gui "Did not find qt5")
else()

include_directories (
	${CMAKE_SOURCE_DIR} 
	${CMAKE_CURRENT_BINARY_DIR}
	${Qt5Quick_INCLUDE_DIRS} 
	${Qt5Gui_INCLUDE_DIRS} 
	${Qt5Core_INCLUDE_DIRS} 
	${Qt5Qml_INCLUDE_DIRS} 
	${Qt5Widgets_INCLUDE_DIRS}    
	${Qt5Test_INCLUDE_DIRS}
	../kdb
	../../libtools/include
	../../libtools/include/merging
	../../libtools/include/helper
	)

set(qt-gui_HDRS
	src/visitor.hpp
	src/printvisitor.hpp
	src/keysetvisitor.hpp
	src/newkeycommand.hpp
	src/editkeycommand.hpp
	src/deletekeycommand.hpp
	src/cutkeycommand.hpp
	src/copykeycommand.hpp
	src/importconfigurationcommand.hpp
	)

add_headers (qt-gui_HDRS)                                               
add_cppheaders (qt-gui_HDRS)                                            
add_toolheaders (qt-gui_HDRS)

file(GLOB kdb_SRCS "../kdb/*.cpp")
file(GLOB qt-gui_SRCS "src/*.cpp")

set(qt-gui_RSCS
	resources.qrc
	)
	
qt5_add_resources(RSCS ${qt-gui_RSCS})

add_executable(qt-gui ${qt-gui_SRCS} ${kdb_SRCS} ${qt-gui_HDRS}
	${UIS} ${RSCS} ${TRS})

qt5_use_modules(qt-gui Quick Gui Core Qml Widgets)

target_link_libraries(qt-gui ${Qt5Quick_LIBRARIES} ${Qt5Gui_LIBRARIES}
	${Qt5Core_LIBRARIES} ${Qt5Qml_LIBRARIES} ${Qt5Widgets_LIBRARIES}
	${Qt5Test_LIBRARIES} elektra elektratools)

install(TARGETS qt-gui DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

endif()
