#ifndef GUIBACKEND_HPP
#define GUIBACKEND_HPP

#include <QObject>
#include <QStringList>
#include <backend.hpp>

class GUIBackend : public QObject
{
	Q_OBJECT

public:
	explicit GUIBackend(QObject *parent = 0);
	GUIBackend(const GUIBackend &other);

	Q_INVOKABLE void			createBackend(const QString &mountpoint);

	Q_INVOKABLE void			addPath(const QString &path);

	Q_INVOKABLE void			addPlugin(QString name, QStringList config);

	Q_INVOKABLE QString			pluginInfo(QString pluginName) const;

	Q_INVOKABLE QString			mountPoints() const;

	Q_INVOKABLE QStringList 	availablePlugins(bool includeStorage, bool includeResolver) const;

	Q_INVOKABLE QStringList 	nameFilters();

	Q_INVOKABLE void			serialise();

	Q_INVOKABLE bool			validated();

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
