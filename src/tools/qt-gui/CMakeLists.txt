set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(DISCOUNT_FIND_QUIETLY ON)

set(QCOMPONENTS Core Quick Gui Qml Widgets Test)
find_package(Qt5 COMPONENTS ${QCOMPONENTS})
find_package(Qt5 OPTIONAL_COMPONENTS Svg QUIET)
find_package(Discount)

if (NOT Qt5_FOUND)
	remove_tool(qt-gui "qt5 (Components: ${QCOMPONENTS}) not found")
elseif (NOT DISCOUNT_FOUND)
	remove_tool(qt-gui "discount or libmarkdown-dev not found")
else()

if (NOT Qt5Svg_FOUND)
	message(WARNING "No Qt5 svg module found. Svg icons themes will not be supported.")
endif ()

include_directories (
	${CMAKE_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
	)

include_directories (
	SYSTEM
	${Qt5Quick_INCLUDE_DIRS}
	${Qt5Gui_INCLUDE_DIRS}
	${Qt5Core_INCLUDE_DIRS}
	${Qt5Qml_INCLUDE_DIRS}
	${Qt5Widgets_INCLUDE_DIRS}
	${Qt5Test_INCLUDE_DIRS}
	${Qt5Svg_INCLUDE_DIRS}
	src/markdownconverter
	)

file(GLOB qt-gui_HDRS "src/markdownconverter/*.h" "src/markdownconverter/template/*.h")

add_headers (qt-gui_HDRS)
add_cppheaders (qt-gui_HDRS)
add_toolheaders (qt-gui_HDRS)

file(GLOB qt-gui_SRCS "src/*.cpp" "src/markdownconverter/*.cpp" "src/markdownconverter/template/*.cpp")

set(qt-gui_RSCS	resources.qrc)

qt5_add_resources(RSCS ${qt-gui_RSCS})

add_executable(qt-gui ${qt-gui_SRCS} ${qt-gui_HDRS} ${UIS} ${RSCS} ${TRS})
add_dependencies (qt-gui kdberrors_generated)

qt5_use_modules(qt-gui Quick Gui Core Qml Widgets)

include_directories(${DISCOUNT_INCLUDES})
target_link_libraries(qt-gui ${DISCOUNT_LIBRARIES})

target_link_libraries(qt-gui ${Qt5Quick_LIBRARIES} ${Qt5Gui_LIBRARIES}
	${Qt5Core_LIBRARIES} ${Qt5Qml_LIBRARIES} ${Qt5Widgets_LIBRARIES}
	${Qt5Svg_LIBRARIES} ${Qt5Test_LIBRARIES})
tool_link_elektratools(qt-gui elektra-ease)

install(TARGETS qt-gui DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
install(PROGRAMS ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/qt-gui DESTINATION bin RENAME elektra-qt-editor)
install(FILES org.libelektra.elektra-qt-editor.desktop DESTINATION share/applications)
install(FILES org.libelektra.elektra-qt-editor.appdata.xml DESTINATION share/appdata)
install(FILES ../../../doc/images/circle.svg DESTINATION share/icons/hicolor/scalable/apps RENAME elektra.svg)
install(FILES ../../../doc/images/circle-symbolic.svg DESTINATION share/icons/hicolor/scalable/apps RENAME elektra-symbolic.svg)

generate_manpage (kdb-qt-gui FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)

endif()
