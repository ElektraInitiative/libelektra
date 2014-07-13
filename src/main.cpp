#include <QApplication>
#include <QQmlApplicationEngine>
#include <QtQml>
#include <QMetaType>

#include "treeviewmodel.hpp"
#include "confignode.hpp"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    qRegisterMetaType<TreeViewModel>("TreeViewModel");
    qRegisterMetaType<ConfigNode>("ConfigNode");

    QQmlApplicationEngine engine;

    QQmlContext *ctxt = engine.rootContext();
    TreeViewModel *model = new TreeViewModel;
    ctxt->setContextProperty("externTreeModel", QVariant::fromValue(model));

    engine.load(QUrl(QStringLiteral("qrc:/qml/main.qml")));
    return app.exec();
}

