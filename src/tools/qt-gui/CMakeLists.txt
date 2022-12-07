set (CMAKE_AUTOMOC ON)
set (CMAKE_INCLUDE_CURRENT_DIR ON)
set (DISCOUNT_FIND_QUIETLY ON)

if (APPLE)
	list (APPEND CMAKE_PREFIX_PATH "/usr/local/opt/qt5" "/usr/local/opt/qt@5.5") # Provide list of default paths for Homebrew’s Qt
endif (APPLE)

set (QCOMPONENTS Core Quick Gui Qml Widgets Test)
find_package (Qt5 COMPONENTS ${QCOMPONENTS})
find_package (
	Qt5
	OPTIONAL_COMPONENTS Svg
	QUIET)
find_package (
	Qt5
	OPTIONAL_COMPONENTS DBus
	QUIET)
find_package (DISCOUNT)

if (NOT Qt5_FOUND)
	remove_tool (qt-gui "qt5 (Components: ${QCOMPONENTS}) not found")
elseif (NOT DISCOUNT_FOUND)
	remove_tool (qt-gui "discount or libmarkdown-dev not found")
else ()

	if (NOT Qt5Svg_FOUND)
		message (WARNING "No Qt5 svg module found. Svg icons themes will not be supported. (please install libqt5svg5-dev)")
	endif ()

	if (NOT Qt5DBus_FOUND)
		message (WARNING "No Qt5 DBus module found. Listening to DBus will not be supported.")
		add_definitions (-DHAVE_DBUS=0)
	else ()
		add_definitions (-DHAVE_DBUS=1)
	endif ()

	include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})

	include_directories (
		SYSTEM
		${Qt5Quick_INCLUDE_DIRS}
		${Qt5Gui_INCLUDE_DIRS}
		${Qt5Core_INCLUDE_DIRS}
		${Qt5Qml_INCLUDE_DIRS}
		${Qt5Widgets_INCLUDE_DIRS}
		${Qt5Test_INCLUDE_DIRS}
		${Qt5Svg_INCLUDE_DIRS}
		src/markdownconverter)

	file (GLOB qt-gui_HDRS "src/markdownconverter/*.h" "src/markdownconverter/template/*.h")

	add_headers (qt-gui_HDRS)
	add_cppheaders (qt-gui_HDRS)
	add_toolheaders (qt-gui_HDRS)

	file (GLOB qt-gui_SRCS "src/*.cpp" "src/markdownconverter/*.cpp" "src/markdownconverter/template/*.cpp")

	set (qt-gui_RSCS resources.qrc)
	set_property (SOURCE qrc_resources.cpp PROPERTY SKIP_AUTOMOC ON)

	qt5_add_resources (RSCS ${qt-gui_RSCS})

	add_executable (qt-gui ${qt-gui_SRCS} ${qt-gui_HDRS} ${UIS} ${RSCS} ${TRS})

	target_link_libraries (qt-gui Qt5::Quick Qt5::Gui Qt5::Core Qt5::Qml Qt5::Widgets)

	include_directories (${DISCOUNT_INCLUDES})
	target_link_libraries (qt-gui ${DISCOUNT_LIBRARIES})

	target_link_libraries (
		qt-gui
		${Qt5Quick_LIBRARIES}
		${Qt5Gui_LIBRARIES}
		${Qt5Core_LIBRARIES}
		${Qt5Qml_LIBRARIES}
		${Qt5Widgets_LIBRARIES}
		${Qt5Svg_LIBRARIES}
		${Qt5Test_LIBRARIES}
		${Qt5DBus_LIBRARIES})
	tool_link_elektratools (qt-gui elektra-ease)

	install (
		TARGETS qt-gui
		DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
		COMPONENT elektra-qt-gui)
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-qt-editor.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra-qt-editor")
	install (
		PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/elektra-qt-editor"
		DESTINATION bin
		COMPONENT elektra-qt-gui)
	install (
		FILES org.libelektra.elektra-qt-editor.desktop
		DESTINATION share/applications
		COMPONENT elektra-qt-gui)
	install (
		FILES org.libelektra.elektra-qt-editor.appdata.xml
		DESTINATION share/appdata
		COMPONENT elektra-qt-gui)
	install (
		FILES ../../../doc/images/logo/logo_color.svg
		DESTINATION share/icons/hicolor/scalable/apps
		RENAME elektra.svg
		COMPONENT elektra-qt-gui)
	install (
		FILES ../../../doc/images/logo/logo_gray.svg
		DESTINATION share/icons/hicolor/scalable/apps
		RENAME elektra-symbolic.svg
		COMPONENT elektra-qt-gui)

	generate_man_page (kdb-qt-gui FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md COMPONENT elektra-qt-gui)

endif ()
