set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_FLAGS "${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")

set(CMAKE_BUILD_TYPE Debug)
set(DISCOUNT_FIND_QUIETLY ON)

add_definitions(-Wall)

set(QCOMPONENTS Core Quick Gui Qml Widgets Test)
find_package(Qt5 COMPONENTS ${QCOMPONENTS})
find_package(Discount)

if (NOT Qt5_FOUND)
	remove_tool(qt-gui "qt5 (Components: ${QCOMPONENTS}) not found")
elseif (NOT DISCOUNT_FOUND)
	remove_tool(qt-gui "discount (libmarkdown) not found")
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
	src/markdownconverter
	../../libtools/include
	../../libtools/include/merging
	../../libtools/include/helper
	)

file(GLOB qt-gui_HDRS "../kdb/*.hpp" "src/markdownconverter/*.h" "src/markdownconverter/template/*.h")

add_headers (qt-gui_HDRS)
add_cppheaders (qt-gui_HDRS)
add_toolheaders (qt-gui_HDRS)

file(GLOB kdb_SRCS "../kdb/*.cpp")
file(GLOB kdb_NO_SRCS ../kdb/main.cpp ../kdb/factory.cpp)
list(REMOVE_ITEM kdb_SRCS ${kdb_NO_SRCS})

file(GLOB qt-gui_SRCS "src/*.cpp" "src/markdownconverter/*.cpp" "src/markdownconverter/template/*.cpp")

set(qt-gui_RSCS	resources.qrc)

qt5_add_resources(RSCS ${qt-gui_RSCS})

add_executable(qt-gui ${qt-gui_SRCS} ${kdb_SRCS} ${qt-gui_HDRS} ${UIS} ${RSCS} ${TRS})

qt5_use_modules(qt-gui Quick Gui Core Qml Widgets)

include_directories(${DISCOUNT_INCLUDES})
target_link_libraries(qt-gui ${DISCOUNT_LIBRARIES})

target_link_libraries(qt-gui ${Qt5Quick_LIBRARIES} ${Qt5Gui_LIBRARIES}
	${Qt5Core_LIBRARIES} ${Qt5Qml_LIBRARIES} ${Qt5Widgets_LIBRARIES}
	${Qt5Test_LIBRARIES} elektra elektratools markdown)

install(TARGETS qt-gui DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

endif()
