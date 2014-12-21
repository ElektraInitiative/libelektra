#ifndef GUIBACKEND_HPP
#define GUIBACKEND_HPP

#include <QObject>
#include <QStringList>
#include <backend.hpp>
#include "treeviewmodel.hpp"

class GUIBackend : public QObject
{
	Q_OBJECT

public:
	explicit GUIBackend(QObject *parent = 0);
	GUIBackend(const GUIBackend &other);



	/**
	 * @brief Creates a new backend on a mountpoint.
	 *
	 * @param mountpoint The mountpoint of the new backend.
	 */
	Q_INVOKABLE void			createBackend(const QString &mountpoint);

	/**
	 * @brief Add a path
	 *
	 * @param path ...
	 * @return void
	 */
	Q_INVOKABLE void			addPath(const QString &path);

	/**
	 * @brief ...
	 *
	 * @param name ...
	 * @param config ...
	 * @return void
	 */
	Q_INVOKABLE void			addPlugin(QString name, TreeViewModel *pluginConfig);

	/**
	 * @brief ...
	 *
	 * @param pluginName ...
	 * @return QString
	 */
	Q_INVOKABLE QString			pluginInfo(QString pluginName) const;

	/**
	 * @brief ...
	 *
	 * @return QString
	 */
	Q_INVOKABLE QString			mountPoints() const;

	/**
	 * @brief ...
	 *
	 * @param includeStorage ...
	 * @param includeResolver ...
	 * @return QStringList
	 */
	Q_INVOKABLE QStringList 	availablePlugins(bool includeStorage, bool includeResolver) const;

	/**
	 * @brief ...
	 *
	 * @return QStringList
	 */
	Q_INVOKABLE QStringList 	nameFilters();

	/**
	 * @brief ...
	 *
	 * @return void
	 */
	Q_INVOKABLE void			serialise();

	/**
	 * @brief ...
	 *
	 * @return bool
	 */
	Q_INVOKABLE bool			validated();

	/**
	 * @brief ...
	 *
	 * @return void
	 */
	Q_INVOKABLE void			deleteBackend();

private:
	kdb::tools::Backend*	m_backend;
	kdb::KeySet				m_mountConf;
	kdb::KDB				m_kdb;
	QString					m_name;

signals:
	void showMessage(QString title, QString text, QString detailedText) const;
};

#endif // GUIBACKEND_HPP
