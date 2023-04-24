/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef GUIBACKEND_HPP
#define GUIBACKEND_HPP

#include "./treeviewmodel.hpp"
#include <QObject>
#include <QStringList>
#include <backendbuilder.hpp>

/**
 * @brief The GUIBackend class. It interacts with the wizard in the GUI to create new backends.
 */

class GUIBackend : public QObject
{
	Q_OBJECT

public:
	/**
	 * @brief GUIBackend The default constructor.
	 * @param parentBackend An optional parent backend.
	 */
	explicit GUIBackend (QObject * parentBackend = nullptr);

	/**
	 * @brief GUIBackend The mandatory copy constructor.
	 */
	GUIBackend (const GUIBackend & other) : QObject ()
	{
		Q_UNUSED (other)
	}

	/**
	 * @brief Creates a new backend on a mountpoint.
	 *
	 * @param mountpoint The mountpoint of the new backend.
	 */
	Q_INVOKABLE void createBackend (const QString & mountpoint);

	/**
	 * @brief Add path to a backend fallback file.
	 *
	 * @param path The path to a backend fallback file.
	 */
	Q_INVOKABLE void addPath (const QString & path);

	/**
	 * @brief Add a plugin to a backend.
	 *
	 * @param name The name of the plugin.
	 * @param recommended Wether to include recommended plugins.
	 */
	Q_INVOKABLE void addPlugin (QString plugin, bool recommended);

	/**
	 * @brief Remove a plugin from a backend.
	 *
	 * @param name The name of the plugin.
	 */
	Q_INVOKABLE void removePlugin (QString plugin);

	/**
	 * @brief Provides information about a plugin.
	 *
	 * @param plugin The plugin.
	 * @return The information about the plugin.
	 */
	Q_INVOKABLE QString pluginInfo (QString plugin) const;

	/**
	 * @brief Returns a list of currently used mountpoints.
	 *
	 * @return A list of currently used mountpoints.
	 */
	Q_INVOKABLE QString mountPoints () const;

	/**
	 * @brief Returns a list of all currently available plugins.
	 *
	 * @param includeStorage Determines if storage plugins should be included in the list.
	 * @param includeResolver Determines if resolver plugins should be included in the list.
	 *
	 * @return A list of all currently available plugins.
	 */
	Q_INVOKABLE QStringList availablePlugins (bool includeStorage, bool includeResolver) const;

	/**
	 * @brief Returns a list of all currently available namefilters.
	 *
	 * @return A list of all currently available namefilters.
	 */
	Q_INVOKABLE QStringList nameFilters ();

	/**
	 * @brief Writes the current backend permanently to storage.
	 */
	Q_INVOKABLE void serialise (TreeViewModel * model);

	/**
	 * @brief Returns if the current backend is validated.
	 *
	 * @return If the current backend is validated.
	 */
	Q_INVOKABLE bool validated ();

	/**
	 * @brief pluginConfigModel Contains all keys that provide additonal configuration for a plugin.
	 * @return All keys that provide additonal configuration for a plugin.
	 */
	Q_INVOKABLE TreeViewModel * pluginConfigModel () const;

	/**
	 * @brief Return the already added plugins of the backend as QStringList.
	 *
	 * @return The already added plugins of the backend as QStringList.
	 */
	Q_INVOKABLE QStringList addedPlugins () const;

	/**
	 * @brief Returns if a plugin is already added to the backend.
	 *
	 * @param plugin The plugin
	 * @return true if the plugin is already added to the backend
	 */
	Q_INVOKABLE bool pluginAlreadyAdded (QString plugin) const;

	/**
	 * @brief Returns the type of a plugin.
	 *
	 * @param plugin The plugin
	 * @return The type of a plugin.
	 */
	Q_INVOKABLE QString pluginType (QString plugin) const;

private:
	QSharedPointer<kdb::tools::MountBackendBuilder> m_backend;
	kdb::KeySet m_mountConf;
	kdb::KDB m_kdb;
	QString m_name;
	TreeViewModel * m_pluginConfigModel;

	/**
	 * @brief resetModel Clears the TreeViewModel that contains the keys for additional configuration for a plugin.
	 */
	void resetModel ();

signals:
	/**
	 * @brief Triggers a messagedialog in the GUI.
	 * @param title The title of the messagedialog in the GUI.
	 * @param text The text of the messagedialog in the GUI.This is the text that will be initially shown to the user.
	 * @param detailedText The detailed text of the messagedialog in the GUI.The user will have to click on a button to access this
	 * text.
	 */
	void showMessage (QString title, QString text, QString detailedText) const;
};

#endif // GUIBACKEND_HPP
