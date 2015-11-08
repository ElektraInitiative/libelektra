#include <QApplication>
#include <QQmlApplicationEngine>
#include <QDebug>
#include <QTranslator>
#include <QtQml>
#include <QMetaType>
#include <QtTest/qtestcase.h>
#include <kdb.hpp>

#include "treeviewmodel.hpp"
#include "confignode.hpp"
#include "undomanager.hpp"
#include "guibackend.hpp"
#include "guisettings.hpp"
#include "datacontainer.hpp"

int main(int argc, char* argv[])
{
	QApplication app(argc, argv);

	qRegisterMetaType<TreeViewModel>("TreeViewModel");
	qRegisterMetaType<ConfigNode>	("ConfigNode");
	qRegisterMetaType<UndoManager>	("UndoManager");
	qRegisterMetaType<GUIBackend>	("GUIBackend");
	qRegisterMetaType<GUISettings>	("GUISettings");
	qmlRegisterType<DataContainer>	("org.libelektra.qtgui", 1, 0, "DataContainer");

	QString locale = QLocale::system().name();

	QTranslator translator;
	translator.load(QString(":/qml/i18n/lang_") + locale + QString(".qm"));
	app.installTranslator(&translator);

	QQmlApplicationEngine engine;
	QQmlContext* ctxt = engine.rootContext();

	UndoManager manager;
	GUIBackend	backend;
	GUISettings settings;
	kdb::KDB kdb;
	TreeViewModel treeModel(&kdb);

	engine.setObjectOwnership(&treeModel, QQmlApplicationEngine::CppOwnership);

	ctxt->setContextProperty("undoManager", &manager);
	ctxt->setContextProperty("externTreeModel", &treeModel);
	ctxt->setContextProperty("guiBackend", &backend);
	ctxt->setContextProperty("guiSettings", &settings);

	treeModel.populateModel();

	engine.load(QUrl(QStringLiteral("qrc:/qml/main.qml")));

	return app.exec();
}
