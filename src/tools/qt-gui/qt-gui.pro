QT += quick  gui core  qml  widgets testlib

HEADERS += \
	src/treeviewmodel.hpp \
	src/confignode.hpp \
	src/visitor.hpp \
	src/printvisitor.hpp \
	src/keysetvisitor.hpp \
	src/undomanager.hpp \
	src/newkeycommand.hpp \
	src/editkeycommand.hpp \
	src/deletekeycommand.hpp \
	src/cutkeycommand.hpp \
	src/copykeycommand.hpp \
	src/importconfigurationcommand.hpp \
	../kdb/check.hpp \
	../kdb/cmdline.hpp \
	../kdb/command.hpp \
	../kdb/convert.hpp \
	../kdb/cp.hpp \
	../kdb/export.hpp \
	../kdb/external.hpp \
	../kdb/factory.hpp \
	../kdb/file.hpp \
	../kdb/fstab.hpp \
	../kdb/get.hpp \
	../kdb/import.hpp \
	../kdb/info.hpp \
	../kdb/ls.hpp \
	../kdb/merge.hpp \
	../kdb/mergehelper.hpp \
	../kdb/metaget.hpp \
	../kdb/metals.hpp \
	../kdb/metaset.hpp \
	../kdb/mount.hpp \
	../kdb/mountbase.hpp \
	../kdb/mv.hpp \
	../kdb/remount.hpp \
	../kdb/rename.hpp \
	../kdb/rm.hpp \
	../kdb/set.hpp \
	../kdb/sget.hpp \
	../kdb/shell.hpp \
	../kdb/test.hpp \
	../kdb/umount.hpp \
	../kdb/validation.hpp \
	src/markdownconverter/template/htmltemplate.h \
	src/markdownconverter/template/template.h \
	src/markdownconverter/discountmarkdownconverter.h \
	src/markdownconverter/markdownconverter.h \
	src/markdownconverter/markdowndocument.h \
    ../kdb/list.hpp

SOURCES += \
	src/main.cpp \
	src/treeviewmodel.cpp \
	src/confignode.cpp \
	src/printvisitor.cpp \
	src/keysetvisitor.cpp \
	src/undomanager.cpp \
	src/newkeycommand.cpp \
	src/deletekeycommand.cpp \
	src/editkeycommand.cpp \
	src/copykeycommand.cpp \
	src/cutkeycommand.cpp \
	src/importconfigurationcommand.cpp \
	../kdb/check.cpp \
	../kdb/cmdline.cpp \
	../kdb/command.cpp \
	../kdb/convert.cpp \
	../kdb/cp.cpp \
	../kdb/export.cpp \
	../kdb/external.cpp \
	../kdb/file.cpp \
	../kdb/fstab.cpp \
	../kdb/get.cpp \
	../kdb/import.cpp \
	../kdb/info.cpp \
	../kdb/ls.cpp \
	../kdb/merge.cpp \
	../kdb/mergehelper.cpp \
	../kdb/metaget.cpp \
	../kdb/metals.cpp \
	../kdb/metaset.cpp \
	../kdb/mount.cpp \
	../kdb/mountbase.cpp \
	../kdb/mv.cpp \
	../kdb/remount.cpp \
	../kdb/rm.cpp \
	../kdb/set.cpp \
	../kdb/sget.cpp \
	../kdb/shell.cpp \
	../kdb/test.cpp \
	../kdb/umount.cpp \
	../kdb/validation.cpp \
	src/markdownconverter/template/htmltemplate.cpp \
	src/markdownconverter/discountmarkdownconverter.cpp \
    ../kdb/list.cpp

CONFIG += debug

# Additional import path used to resolve QML modules in Qt Creator's code model
QML_IMPORT_PATH =

# Default rules for deployment.
include(deployment.pri)

RESOURCES += \
	resources.qrc
	i18n.qrc

OTHER_FILES += \
	qml/main.qml\
	qml/BasicWindow.qml\
	qml/ButtonRow.qml\
	qml/Page1.qml\
	qml/Page2.qml\
	qml/Page3.qml\
	qml/WizardLoader.qml\
	qml/WizardTemplate.qml\
	qml/Page4.qml\
	qml/NewMetaKey.qml\
	qml/BasicRectangle.qml \
	qml/UnmountBackendWindow.qml \
	qml/NewArrayEntry.qml \
	qml/TreeView.qml \
	qml/KeyWindow.qml \
	qml/NewKeyWindow.qml \
	qml/EditKeyWindow.qml \
	qml/ExitDialog.qml \
	qml/MainMenuBar.qml \
	qml/ExportDialog.qml \
	qml/ImportDialog.qml

SUBDIRS += \
	unittest/unittest.pro \

unix: LIBS += -L/usr/local/lib/ -lelektra
unix: LIBS += -L/usr/local/lib/ -lelektratools
unix: LIBS += -L/usr/local/lib/ -lmarkdown

INCLUDEPATH += /usr/local/include/elektra
INCLUDEPATH += ../kdb/
INCLUDEPATH += src/markdownconverter
INCLUDEPATH += ../../libtools/include/
INCLUDEPATH += ../../include/
