/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <QApplication>
#include <QDebug>
#include <QMessageBox>
#include <QMetaType>
#include <QQmlApplicationEngine>
#include <QTranslator>
#include <QtQml>
#include <QtTest/qtestcase.h>
#include <kdb.hpp>
#include <merging/mergingkdb.hpp>

#include "confignode.hpp"
#include "datacontainer.hpp"
#include "guibackend.hpp"
#include "guisettings.hpp"
#include "treeviewmodel.hpp"
#include "undomanager.hpp"

int main (int argc, char * argv[])
{
	QApplication app (argc, argv);

	qRegisterMetaType<TreeViewModel> ("TreeViewModel");
	qRegisterMetaType<ConfigNode> ("ConfigNode");
	qRegisterMetaType<UndoManager> ("UndoManager");
	qRegisterMetaType<GUIBackend> ("GUIBackend");
	qRegisterMetaType<GUISettings> ("GUISettings");
	qmlRegisterType<DataContainer> ("org.libelektra.qtgui", 1, 0, "DataContainer");

	QString locale = QLocale::system ().name ();

	QTranslator translator;
	translator.load (QString (":/qml/i18n/lang_") + locale + QString (".qm"));
	app.installTranslator (&translator);

	QQmlApplicationEngine engine;
	QQmlContext * ctxt = engine.rootContext ();

	UndoManager manager;
	GUIBackend backend;
	GUISettings settings;
	kdb::tools::merging::MergingKDB kdb;
	TreeViewModel treeModel (&kdb);

	engine.setObjectOwnership (&treeModel, QQmlApplicationEngine::CppOwnership);

	ctxt->setContextProperty ("undoManager", &manager);
	ctxt->setContextProperty ("externTreeModel", &treeModel);
	ctxt->setContextProperty ("guiBackend", &backend);
	ctxt->setContextProperty ("guiSettings", &settings);

	try
	{
		treeModel.populateModel ();
	}
	catch (std::exception const & e)
	{
		QMessageBox msgBox;
		msgBox.setText ("Could not start qt-gui. Failed while reading the whole configuration.");
		msgBox.setInformativeText (e.what ());
		msgBox.setIcon (QMessageBox::Critical);
		msgBox.exec ();
		return 1;
	}

	engine.load (QUrl (QStringLiteral ("qrc:/qml/main.qml")));

	return app.exec ();
}
