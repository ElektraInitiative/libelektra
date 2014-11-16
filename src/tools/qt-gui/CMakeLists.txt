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
	src/markdownconverter
	../../libtools/include
	../../libtools/include/merging
	../../libtools/include/helper
	)

add_headers (qt-gui_HDRS)
add_cppheaders (qt-gui_HDRS)
add_toolheaders (qt-gui_HDRS)

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
	src/markdownconverter/discountmarkdownconverter.h
	src/markdownconverter/template/htmltemplate.h
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
	src/undomanager.cpp
	src/newkeycommand.cpp
	src/deletekeycommand.cpp
	src/editkeycommand.cpp
	src/copykeycommand.cpp
	src/cutkeycommand.cpp
	src/importconfigurationcommand.cpp
	src/markdownconverter/discountmarkdownconverter.cpp
	src/markdownconverter/template/htmltemplate.cpp
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
	../kdb/list.cpp
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
	)

set(qt-gui_RSCS
	resources.qrc
	)

qt5_add_resources(RSCS ${qt-gui_RSCS})

add_executable(qt-gui ${qt-gui_SRCS} ${qt-gui_HDRS} ${UIS} ${RSCS} ${TRS})

qt5_use_modules(qt-gui Quick Gui Core Qml Widgets)

target_link_libraries(qt-gui ${Qt5Quick_LIBRARIES} ${Qt5Gui_LIBRARIES} ${Qt5Core_LIBRARIES} ${Qt5Qml_LIBRARIES} ${Qt5Widgets_LIBRARIES} ${Qt5Test_LIBRARIES} elektra elektratools markdown)

install(TARGETS qt-gui DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

endif()
