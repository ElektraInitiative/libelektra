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

    QList<QObject*> dataList;

    ConfigNode *node1 = new ConfigNode(QString("Node1"), QString("/Node1"));
    ConfigNode *node2 = new ConfigNode(QString("Node2"), QString("/Node1/Node2"));
    ConfigNode *node3 = new ConfigNode(QString("Node3"), QString("/Node1/Node2/Node3"));
    ConfigNode *node4 = new ConfigNode(QString("Node4"), QString("/Node4"));

    node2->appendChild(node3);
    node1->appendChild(node2);

    dataList.append(node1);
    dataList.append(node4);

    QQmlContext *ctxt = engine.rootContext();
    ctxt->setContextProperty("externTreeModel", QVariant::fromValue(dataList));

    engine.load(QUrl(QStringLiteral("qrc:/qml/main.qml")));
    return app.exec();

    //    KeySet config;
    //    KDB kdb;
    //    kdb.get(config, "system");

    //    QStringList dataList;

    //    config.rewind();

    //    while (config.next())
    //        dataList.append(QString::fromStdString(config.current().getName()));

    //    TreeViewModel model;
    //    TreeModel tModel(dataList);

}
