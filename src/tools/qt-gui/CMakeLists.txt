cmake_minimum_required(VERSION 2.8)

project(qt-gui)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_FLAGS "${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")

set(CMAKE_BUILD_TYPE Debug)

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
	../kdb/
	../../libtools/include/
	../../include/
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
	../kdb/check.hpp 
	../kdb/cmdline.hpp 
	../kdb/command.hpp 
	../kdb/convert.hpp 
	../kdb/cp.hpp 
	../kdb/export.hpp 
	../kdb/external.hpp 
	../kdb/factory.hpp 
	../kdb/file.hpp 
	../kdb/fstab.hpp 
	../kdb/get.hpp 
	../kdb/import.hpp 
	../kdb/info.hpp 
	../kdb/ls.hpp 
	../kdb/merge.hpp 
	../kdb/mergehelper.hpp 
	../kdb/metaget.hpp 
	../kdb/metals.hpp 
	../kdb/metaset.hpp 
	../kdb/mount.hpp 
	../kdb/mountbase.hpp 
	../kdb/mv.hpp 
	../kdb/remount.hpp 
	../kdb/rename.hpp 
	../kdb/rm.hpp 
	../kdb/set.hpp 
	../kdb/sget.hpp 
	../kdb/shell.hpp 
	../kdb/test.hpp 
	../kdb/umount.hpp 
	../kdb/validation.hpp 
	../../libtools/include/merging/automergestrategy.hpp
	../../libtools/include/merging/interactivemergestrategy.hpp
	../../libtools/include/merging/mergeconflict.hpp
	../../libtools/include/merging/mergeconflictstrategy.hpp
	../../libtools/include/merging/mergeresult.hpp
	../../libtools/include/merging/mergetask.hpp
	../../libtools/include/merging/metamergestrategy.hpp
	../../libtools/include/merging/newkeystrategy.hpp
	../../libtools/include/merging/onesidestrategy.hpp
	../../libtools/include/merging/onesidevaluestrategy.hpp
	../../libtools/include/merging/threewaymerge.hpp
	../../libtools/include/backend.hpp
	../../libtools/include/backends.hpp
	../../libtools/include/modules.hpp
	../../libtools/include/plugin.hpp
	../../libtools/include/plugins.hpp
	../../libtools/include/toolexcept.hpp
	../../libtools/include/helper/comparison.hpp
	../../libtools/include/helper/keyhelper.hpp
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
	src/importconfigurationcommand.cpp
	../kdb/check.cpp 
	../kdb/cmdline.cpp 
	../kdb/command.cpp 
	../kdb/convert.cpp 
	../kdb/cp.cpp 
	../kdb/export.cpp 
	../kdb/external.cpp 
	../kdb/file.cpp 
	../kdb/fstab.cpp 
	../kdb/get.cpp 
	../kdb/import.cpp 
	../kdb/info.cpp 
	../kdb/ls.cpp 
	../kdb/merge.cpp 
	../kdb/mergehelper.cpp 
	../kdb/metaget.cpp 
	../kdb/metals.cpp 
	../kdb/metaset.cpp 
	../kdb/mount.cpp 
	../kdb/mountbase.cpp 
	../kdb/mv.cpp 
	../kdb/remount.cpp 
	../kdb/rm.cpp 
	../kdb/set.cpp 
	../kdb/sget.cpp 
	../kdb/shell.cpp 
	../kdb/test.cpp 
	../kdb/umount.cpp 
	../kdb/validation.cpp
	../../libtools/src/merging/automergestrategy.cpp
	../../libtools/src/merging/interactivemergestrategy.cpp
	../../libtools/src/merging/mergeconflictstrategy.cpp
	../../libtools/src/merging/mergeresult.cpp
	../../libtools/src/merging/metamergestrategy.cpp
	../../libtools/src/merging/newkeystrategy.cpp
	../../libtools/src/merging/onesidestrategy.cpp
	../../libtools/src/merging/onesidevaluestrategy.cpp
	../../libtools/src/merging/threewaymerge.cpp
	../../libtools/src/backend.cpp
	../../libtools/src/backends.cpp
	../../libtools/src/modules.cpp
	../../libtools/src/plugin.cpp
	../../libtools/src/plugins.cpp
	../../libtools/src/helper/comparison.cpp
	../../libtools/src/helper/keyhelper.cpp
	)

set(qt-gui_RSCS
	resources.qrc
	)
	
qt5_add_resources(RSCS ${qt-gui_RSCS})

add_executable(qt-gui ${qt-gui_SRCS} ${qt-gui_HDRS} ${UIS} ${RSCS} ${TRS})

qt5_use_modules(qt-gui Quick Gui Core Qml Widgets)

target_link_libraries(qt-gui ${Qt5Quick_LIBRARIES} ${Qt5Gui_LIBRARIES} ${Qt5Core_LIBRARIES} ${Qt5Qml_LIBRARIES} ${Qt5Widgets_LIBRARIES} ${Qt5Test_LIBRARIES} elektra elektratools)
