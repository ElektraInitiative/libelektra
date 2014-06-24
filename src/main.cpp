#include <QApplication>
#include <QQmlApplicationEngine>
#include <QtQml>
#include <QMetaType>

#include "treeviewmodel.h"
#include "confignode.h"

int main(int argc, char *argv[])
{

    QApplication app(argc, argv);

    QQmlApplicationEngine engine;

    QQmlContext *ctxt = engine.rootContext();
    TreeViewModel *model = new TreeViewModel(ctxt);
    ctxt->setContextProperty("externTreeModel", QVariant::fromValue(model));

    engine.load(QUrl(QStringLiteral("qrc:/qml/main.qml")));
    return app.exec();
}

