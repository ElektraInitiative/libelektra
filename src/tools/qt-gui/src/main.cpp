#include <QApplication>
#include <QQmlApplicationEngine>
#include <QtQml>
#include <QMetaType>
#include <QtTest/qtestcase.h>
#include <kdb.hpp>

#include "treeviewmodel.hpp"
#include "confignode.hpp"
#include "undomanager.hpp"
#include "guibackend.hpp"

int main(int argc, char* argv[])
{
	QApplication app(argc, argv);

	qRegisterMetaType<TreeViewModel> ("TreeViewModel");
	qRegisterMetaType<ConfigNode> ("ConfigNode");
	qRegisterMetaType<UndoManager> ("UndoManager");
	qRegisterMetaType<GUIBackend> ("GUIBackend");

	QString locale = QLocale::system().name();

	QTranslator tr;
	tr.load(QString(":/qml/i18n/lang_") + locale + QString(".qm"));
	app.installTranslator(&tr);

	QQmlApplicationEngine engine;
	QQmlContext* ctxt = engine.rootContext();

	UndoManager manager;
	kdb::KDB kdb;
	kdb::KeySet config;
	try
	{
		kdb.get(config, "/");
	}
	catch(kdb::KDBException const& e)
	{
		qDebug() << QString::fromStdString(e.what());
	}

	TreeViewModel* model = new TreeViewModel;
	model->setKeySet(config);
	kdb::KeySet pluginConfig;
	pluginConfig.append(kdb::Key("system", KEY_END));
	pluginConfig.append(kdb::Key("user", KEY_END));
	TreeViewModel* pluginConfigModel = new TreeViewModel;
	pluginConfigModel->setKeySet(pluginConfig);
	GUIBackend backend;

	model->populateModel();

	ctxt->setContextProperty("undoManager", &manager);
	ctxt->setContextProperty("externTreeModel", model);
	ctxt->setContextProperty("guiBackend", &backend);
	ctxt->setContextProperty("pluginConfig", pluginConfigModel);

	engine.load(QUrl(QStringLiteral("qrc:/qml/main.qml")));

	return app.exec();
}
