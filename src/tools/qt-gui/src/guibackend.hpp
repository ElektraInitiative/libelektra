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

	Q_INVOKABLE void createBackend(const QString &mountpoint);

	Q_INVOKABLE QString pluginInfo(QString pluginName) const;

	Q_INVOKABLE QString mountPoints() const;

	Q_INVOKABLE QStringList availablePlugins() const;

	Q_INVOKABLE QStringList nameFilters();
private:
	kdb::tools::Backend* m_backend;

signals:
	void showMessage(QString title, QString text, QString informativeText, QString detailedText, QString icon) const;
};

#endif // GUIBACKEND_HPP
