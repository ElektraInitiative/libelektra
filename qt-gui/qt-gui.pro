QT += quick  gui core  qml  widgets testlib

SOURCES += \
    src/main.cpp \
    src/treeviewmodel.cpp \
    src/confignode.cpp \
    src/printvisitor.cpp \
    src/keysetvisitor.cpp \
    modeltest/dynamictreemodel.cpp \
    modeltest/modeltest.cpp \
    src/undomanager.cpp \
    src/newkeycommand.cpp \
    src/deletekeycommand.cpp \
    src/editkeycommand.cpp \
    src/copykeycommand.cpp \
    src/cutkeycommand.cpp

HEADERS += \
    src/treeviewmodel.hpp \
    src/confignode.hpp \
    src/visitor.hpp \
    src/printvisitor.hpp \
    src/keysetvisitor.hpp \
    modeltest/dynamictreemodel.h \
    modeltest/modeltest.h \
    src/undomanager.hpp \
    src/newkeycommand.hpp \
    src/editkeycommand.hpp \
    src/deletekeycommand.hpp \
    src/cutkeycommand.hpp \
    src/copykeycommand.hpp

CONFIG += debug

# Additional import path used to resolve QML modules in Qt Creator's code model
QML_IMPORT_PATH =

INCLUDEPATH += /usr/include/elektra
INCLUDEPATH += /usr/local/include/elektra

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
    qml/EditKeyWindow.qml

unix: CONFIG += link_pkgconfig
unix: PKGCONFIG += elektra

SUBDIRS += \
    unittest/unittest.pro \
#    modeltest/modeltest.pro
