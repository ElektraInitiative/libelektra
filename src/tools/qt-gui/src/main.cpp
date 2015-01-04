#include <QApplication>
#include <QQmlApplicationEngine>
#include <QDebug>
#include <QFuture>
#include <QtConcurrent/QtConcurrentRun>
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
	kdb::KeySet pluginConfig;
	GUIBackend backend;

	try
	{
		kdb.get(config, "/");
	}
	catch(kdb::KDBException const& e)
	{
		std::cerr << e.what();
	}

	TreeViewModel* treeModel = new TreeViewModel;
	TreeViewModel* pluginConfigModel = new TreeViewModel;

	pluginConfig.append(kdb::Key("system", KEY_END));
	pluginConfig.append(kdb::Key("user", KEY_END));

	pluginConfigModel->setKeySet(pluginConfig);
	treeModel->setKeySet(config);

	ctxt->setContextProperty("undoManager", &manager);
	ctxt->setContextProperty("externTreeModel", treeModel);
	ctxt->setContextProperty("guiBackend", &backend);
	ctxt->setContextProperty("pluginConfig", pluginConfigModel);

	//populate model in new thread, else the view is blocked
	QtConcurrent::run(treeModel, &TreeViewModel::populateModel);

	engine.load(QUrl(QStringLiteral("qrc:/qml/SplashScreen.qml")));

	return app.exec();
}
