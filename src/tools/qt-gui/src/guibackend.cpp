#include "guibackend.hpp"

#include <markdowndocument.h>
#include <discountmarkdownconverter.h>
#include <backends.hpp>
#include <vector>
#include <string>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

GUIBackend::GUIBackend(QObject *parent) :
	QObject(parent)
{
}

GUIBackend::GUIBackend(const GUIBackend &other)
{
	Q_UNUSED(other);
}

void GUIBackend::createBackend(const QString &mountpoint)
{
	m_backend = new Backend();

	KeySet mountConf;

	Key parentKey(Backends::mountpointsPath, KEY_END);

	KDB kdb(parentKey);

	try
	{
		kdb.get(mountConf, parentKey);
	}
	catch(KDBException ex)
	{
		emit showMessage(tr("Error"), tr("Could not read configuration."), "", QString(ex.what()), "c");
	}

	try
	{
		m_backend->setMountpoint(Key(mountpoint.toStdString(), KEY_CASCADING_NAME, KEY_END), mountConf);
	}
	catch(MountpointInvalidException ex)
	{
		emit showMessage(tr("Error"), tr("The provided mount point is invalid."), "", ex.what(), "c");
	}
	catch(MountpointAlreadyInUseException ex)
	{
		emit showMessage(tr("Error"), tr("The provided mount point is one of the already used cascading names."), "", ex.what(), "c");
	}

	try
	{
		m_backend->addPlugin("resolver");
	}
	catch(PluginCheckException ex){
		emit showMessage(tr("Error"), tr("Could not add plugin \"resolver\"."), "", ex.what(), "c");
	}
}

QString GUIBackend::mountPoints() const
{
	Key parentKey(Backends::mountpointsPath, KEY_END);
	KeySet mountConf;
	KDB kdb (parentKey);
	kdb.get(mountConf, parentKey);

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

QStringList GUIBackend::availablePlugins() const
{
	QStringList availPlugins;

	vector<string> pluginVector = listAllAvailablePlugins();

	foreach(string s, pluginVector)
		availPlugins.append(QString::fromStdString(s));

	return availPlugins;
}

QStringList GUIBackend::nameFilters()
{
	QStringList nameFilters;
	QStringList plugins = availablePlugins();

	nameFilters.append("ECF (*.ecf)");

	if(plugins.contains("xmltool"))
		plugins.append("XML (*.xml)");
	if(plugins.contains("ini"))
		plugins.append("INI (*.ini)");

	return nameFilters;
}
