/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

#include "./QQuickThemeIconProvider.hpp"

#include "./confignode.hpp"
#include "./datacontainer.hpp"
#include "./guibackend.hpp"
#include "./guisettings.hpp"
#include "./treeviewmodel.hpp"
#include "./undomanager.hpp"

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

	app.setOrganizationName ("Elektra Initiative");
	app.setOrganizationDomain ("libelektra.org");
	app.setApplicationName ("org.libelektra.elektra-qt-editor");

	QQmlApplicationEngine engine;
	QQmlContext * ctxt = engine.rootContext ();
	engine.addImageProvider (QLatin1String ("theme"), new QQuickThemeIconProvider);

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
