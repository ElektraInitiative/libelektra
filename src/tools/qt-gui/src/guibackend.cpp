#include "guibackend.hpp"

#include <markdowndocument.h>
#include <discountmarkdownconverter.h>
#include <backends.hpp>
#include <vector>
#include <string>
#include <QDebug>
#include <QRegExp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

GUIBackend::GUIBackend(QObject *parent) :
	QObject(parent)
{
}

GUIBackend::GUIBackend(const GUIBackend &other)
	: QObject()
{
	Q_UNUSED(other);
}

void GUIBackend::createBackend(const QString &mountpoint)
{
	m_backend = new Backend();

	Key parentKey(Backends::mountpointsPath, KEY_END);

	try
	{
		m_kdb.get(m_mountConf, parentKey);
	}
	catch(KDBException const& ex)
	{
		emit showMessage(tr("Error"), tr("Could not read from configuration."), QString(ex.what()));
	}

	try
	{
		m_backend->setMountpoint(Key(mountpoint.toStdString(), KEY_CASCADING_NAME, KEY_END), m_mountConf);
	}
	catch(MountpointInvalidException const& ex)
	{
		emit showMessage(tr("Error"), tr("The provided mount point is invalid."), ex.what());
	}
	catch(MountpointAlreadyInUseException const& ex)
	{
		emit showMessage(tr("Error"), tr("The provided mount point is one of the already used cascading names."), ex.what());
	}
}

void GUIBackend::addPath(const QString &path)
{
	try
	{
		m_backend->useConfigFile(path.toStdString());
	}
	catch(FileNotValidException const& ex)
	{
		emit showMessage(tr("Error"), tr("The file you have entered is not valid."), ex.what());
	}
	catch(MissingSymbol const& ex)
	{
		emit showMessage(tr("Error"), tr("Could not add file."), ex.what());
	}
}

void GUIBackend::addPlugin(QString name, QStringList config)
{
	name.chop(name.length() - name.indexOf("[") + 1);

	KDB kdb;
	KeySet globalConf;
	KeySet pluginConf;

	try
	{
		kdb.get(globalConf, "/");
	}
	catch(KDBException const& ex)
	{
		emit showMessage(tr("Error"), tr("Could not read from configuration."), QString(ex.what()));
	}

	foreach(QString s, config)
	{
		Key k = globalConf.lookup(s.toStdString());
		pluginConf.append(k.dup());
	}

	try
	{
		m_backend->addPlugin(name.toStdString(), pluginConf);
	}
	catch(PluginCheckException const &ex)
	{
		emit showMessage(tr("Error"), tr("Could not add plugin \"%1\".").arg(name), ex.what());
	}
}

void GUIBackend::serialise()
{
	Key rootKey (Backends::mountpointsPath, KEY_END);

	try
	{
		m_backend->serialise(rootKey, m_mountConf);
	}
	catch(ToolException &ex)
	{
		emit showMessage(tr("Error"), tr("Could not serialise backend."), ex.what());
	}

	try
	{
		m_kdb.set(m_mountConf, rootKey);
	}
	catch (kdb::KDBException const& ex)
	{
		emit showMessage(tr("Error"), tr("Could not write backend to configuration."), ex.what());
	}
}

bool GUIBackend::validated()
{
	return m_backend->validated();
}

void GUIBackend::deleteBackend()
{
	delete m_backend;
}

QString GUIBackend::mountPoints() const
{
	Key parentKey(Backends::mountpointsPath, KEY_END);
	KeySet mountConf;
	KDB kdb (parentKey);

	try
	{
		kdb.get(mountConf, parentKey);
	}
	catch(KDBException const& ex)
	{
		emit showMessage(tr("Error"), tr("Could not read from configuration."), QString(ex.what()));
		return "";
	}

	QStringList mountPoints;
	mountPoints.append("system/elektra");

	mountConf.rewind();

	Key cur;

	while ((cur = mountConf.next()))
	{
		if (cur.getBaseName() == "mountpoint")
		{
			if (cur.getString().at(0) == '/')
			{
				mountPoints.append(QString::fromStdString("user" + cur.getString()));
				mountPoints.append(QString::fromStdString("system" + cur.getString()));
			}
			else
			{
				mountPoints.append(QString::fromStdString(cur.getString()));
			}

		}

	}
	return mountPoints.join(", ");
}

QString GUIBackend::pluginInfo(QString pluginName) const
{
	Modules modules;
	KeySet info;
	QString infoString;

	pluginName.chop(pluginName.length() - pluginName.indexOf("[") + 1);

	PluginPtr plugin = modules.load(pluginName.toStdString());

	info = plugin->getInfo();

	Key root;
	root.setName(std::string("system/elektra/modules/") + plugin->name() + "/infos");
	Key k = info.lookup(root);

	if (k)
	{
		while ((k = info.next()) && k.getDirName() == root.getName())
		{
			infoString.append(QString::fromStdString(k.getBaseName()) + ": " + QString::fromStdString(k.getString()) + "\n\n");
		}
	}
	else
		infoString.append(tr("No information found."));

	DiscountMarkdownConverter dmc;
	infoString = dmc.renderAsHtml(dmc.createDocument(infoString, DiscountMarkdownConverter::NoImagesOption));

	return infoString;
}

QStringList GUIBackend::availablePlugins(bool includeStorage, bool includeResolver) const
{
	QStringList availPlugins;
	Modules modules;
	PluginPtr ptr;
	QString type;

	vector<string> pluginVector = listAllAvailablePlugins();

	foreach(string s, pluginVector){
		try
		{
			ptr = modules.load(s);
		}
		catch(NoPlugin ex)
		{
			break;
		}

		ptr->loadInfo();
		type = QString::fromStdString(ptr->lookupInfo("provides"));

		if(!((!includeStorage && type == "storage") || (!includeResolver && type == "resolver"))){
			availPlugins.append(QString::fromStdString(s) + QString::fromStdString(" [%1]").arg(type));
		}
	}

	availPlugins.sort();

	return availPlugins;
}

QStringList GUIBackend::nameFilters()
{
	QStringList nameFilters;
	QStringList plugins = availablePlugins(true, false);

	plugins = plugins.filter("[storage]");
	plugins.replaceInStrings(QRegExp("\\s\\[\\w*\\]"), "");

	foreach(QString plugin, plugins)
	{
		QString pattern;

		if(plugin == "ini" || plugin == "simpleini" || plugin == "ni")
			pattern = "*.ini";
		else if(plugin == "xmltool")
			pattern = "*.xml";
		else if(plugin == "tcl")
			pattern = "*.tcl";
		else if(plugin == "yajl")
			pattern = "*.json";
		else if(plugin == "dump")
			pattern = "*.ecf";
		else
			pattern = "*";

			nameFilters.append(QString("%1 (%2)").arg(plugin, pattern));
	}

	nameFilters.sort();

	return nameFilters;
}
