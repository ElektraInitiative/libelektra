QT += quick  gui core  qml  widgets testlib

HEADERS +=  confignodetest.hpp \
	    treeviewtest.hpp \
	    ../src/treeviewmodel.hpp \
	    ../src/confignode.hpp \
	    ../src/visitor.hpp \
	    ../src/printvisitor.hpp \
	    ../src/keysetvisitor.hpp \
	    ../src/undomanager.hpp \
	    ../src/newkeycommand.hpp \
	    ../src/editkeycommand.hpp \
	    ../src/deletekeycommand.hpp \
	    ../src/cutkeycommand.hpp \
	    ../src/copykeycommand.hpp \
	    ../src/importconfigurationcommand.hpp \
	    ../../kdb/check.hpp \
	    ../../kdb/cmdline.hpp \
	    ../../kdb/command.hpp \
	    ../../kdb/convert.hpp \
	    ../../kdb/cp.hpp \
	    ../../kdb/export.hpp \
	    ../../kdb/external.hpp \
	    ../../kdb/factory.hpp \
	    ../../kdb/file.hpp \
	    ../../kdb/fstab.hpp \
	    ../../kdb/get.hpp \
	    ../../kdb/import.hpp \
	    ../../kdb/info.hpp \
	    ../../kdb/ls.hpp \
	    ../../kdb/merge.hpp \
	    ../../kdb/mergehelper.hpp \
	    ../../kdb/metaget.hpp \
	    ../../kdb/metals.hpp \
	    ../../kdb/metaset.hpp \
	    ../../kdb/mount.hpp \
	    ../../kdb/mountbase.hpp \
	    ../../kdb/mv.hpp \
	    ../../kdb/remount.hpp \
	    ../../kdb/rename.hpp \
	    ../../kdb/rm.hpp \
	    ../../kdb/set.hpp \
	    ../../kdb/sget.hpp \
	    ../../kdb/shell.hpp \
	    ../../kdb/test.hpp \
	    ../../kdb/umount.hpp \
	    ../../kdb/validation.hpp \


SOURCES +=  confignodetest.cpp \
	    treeviewtest.cpp \
	    ../src/treeviewmodel.cpp \
	    ../src/confignode.cpp \
	    ../src/printvisitor.cpp \
	    ../src/keysetvisitor.cpp \
	    ../src/undomanager.cpp \
	    ../src/newkeycommand.cpp \
	    ../src/deletekeycommand.cpp \
	    ../src/editkeycommand.cpp \
	    ../src/copykeycommand.cpp \
	    ../src/cutkeycommand.cpp \
	    ../src/importconfigurationcommand.cpp \
	    ../../kdb/check.cpp \
	    ../../kdb/cmdline.cpp \
	    ../../kdb/command.cpp \
	    ../../kdb/convert.cpp \
	    ../../kdb/cp.cpp \
	    ../../kdb/export.cpp \
	    ../../kdb/external.cpp \
	    ../../kdb/file.cpp \
	    ../../kdb/fstab.cpp \
	    ../../kdb/get.cpp \
	    ../../kdb/import.cpp \
	    ../../kdb/info.cpp \
	    ../../kdb/ls.cpp \
	    ../../kdb/merge.cpp \
	    ../../kdb/mergehelper.cpp \
	    ../../kdb/metaget.cpp \
	    ../../kdb/metals.cpp \
	    ../../kdb/metaset.cpp \
	    ../../kdb/mount.cpp \
	    ../../kdb/mountbase.cpp \
	    ../../kdb/mv.cpp \
	    ../../kdb/remount.cpp \
	    ../../kdb/rm.cpp \
	    ../../kdb/set.cpp \
	    ../../kdb/sget.cpp \
	    ../../kdb/shell.cpp \
	    ../../kdb/test.cpp \
	    ../../kdb/umount.cpp \
	    ../../kdb/validation.cpp \

LIBS += -L/usr/local/lib/ -lelektra
LIBS += -L/usr/local/lib/ -lelektratools

INCLUDEPATH += .
INCLUDEPATH += /usr/local/include/elektra
INCLUDEPATH += ../../kdb/
INCLUDEPATH += ../../../libtools/include/
INCLUDEPATH += ../../../include/
