#include <QApplication>
#include <QQmlApplicationEngine>
#include <QtQml>
#include <QDebug>
#include <string>
#include <kdb.hpp>
#include <keyio.hpp>

#include "treeviewmodel.h"
#include "confignode.h"
#include "treemodel.h"

int main(int argc, char *argv[])
{

    QApplication app(argc, argv);

    QQmlApplicationEngine engine;

    TreeViewModel model;

    QQmlContext *ctxt = engine.rootContext();
    ctxt->setContextProperty("externTreeModel", model.getModel());

    engine.load(QUrl(QStringLiteral("qrc:/qml/main.qml")));
    return app.exec();
}
