cmake_minimum_required(VERSION 2.8)

project(qt-gui)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_FLAGS "${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")

set( CMAKE_BUILD_TYPE Debug )

add_definitions(-Wall)

find_package(Qt5 REQUIRED Quick Gui Core Qml Widgets Test)

include_directories (
	${CMAKE_SOURCE_DIR} 
	${CMAKE_CURRENT_BINARY_DIR}
	${Qt5Quick_INCLUDE_DIRS} 
	${Qt5Gui_INCLUDE_DIRS} 
	${Qt5Core_INCLUDE_DIRS} 
	${Qt5Qml_INCLUDE_DIRS} 
	${Qt5Widgets_INCLUDE_DIRS}    
	${Qt5Test_INCLUDE_DIRS}
	/usr/include/elektra
	/usr/local/include/elektra
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
	)

set(qt-gui_SRCS
	src/main.cpp
	src/treeviewmodel.cpp
	src/confignode.cpp
	src/printvisitor.cpp
	src/keysetvisitor.cpp
	modeltest/dynamictreemodel.cpp
	modeltest/modeltest.cpp
	src/undomanager.cpp
	src/newkeycommand.cpp
	src/deletekeycommand.cpp
	src/editkeycommand.cpp
	src/copykeycommand.cpp
	src/cutkeycommand.cpp
	)

set(qt-gui_RSCS
	resources.qrc
	)
	
qt5_add_resources(RSCS ${qt-gui_RSCS})

add_executable(qt-gui ${qt-gui_SRCS} ${UIS} ${RSCS} ${TRS})

qt5_use_modules(qt-gui Quick Gui Core Qml Widgets)

target_link_libraries(qt-gui ${Qt5Quick_LIBRARIES} ${Qt5Gui_LIBRARIES} ${Qt5Core_LIBRARIES} ${Qt5Qml_LIBRARIES} ${Qt5Widgets_LIBRARIES} ${Qt5Test_LIBRARIES} elektra elektratools)
