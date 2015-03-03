#ifndef DATACONTAINER_HPP
#define DATACONTAINER_HPP

#include <QObject>
#include "treeviewmodel.hpp"

class DataContainer : public QObject
{
	Q_OBJECT

public:
	explicit DataContainer(QObject *parent = 0) : QObject(parent) {}
	DataContainer(const DataContainer &other) : QObject() {Q_UNUSED(other)}

	Q_INVOKABLE QString		oldName() const;
	Q_INVOKABLE void		setOldName(const QString &oldName);

	Q_INVOKABLE QString		oldValue() const;
	Q_INVOKABLE void		setOldValue(const QString &oldValue);

	Q_INVOKABLE QVariantMap oldMetadata() const;
	Q_INVOKABLE void		setOldMetadata(TreeViewModel *oldMetadata);

	Q_INVOKABLE QString		newName() const;
	Q_INVOKABLE void		setNewName(const QString &newName);

	Q_INVOKABLE QString		newValue() const;
	Q_INVOKABLE void		setNewValue(const QString &newValue);

	Q_INVOKABLE QVariantMap newMetadata() const;
	Q_INVOKABLE void		setNewMetadata(const QVariantMap &newMetadata);

	Q_INVOKABLE QString		importName() const;
	Q_INVOKABLE void		setImportName(const QString &importName);

	Q_INVOKABLE QString		format() const;
	Q_INVOKABLE void		setFormat(const QString &format);

	Q_INVOKABLE QString		file() const;
	Q_INVOKABLE void		setFile(const QString &file);

	Q_INVOKABLE QString		mergeStrategy() const;
	Q_INVOKABLE void		setMergeStrategy(const QString &mergeStrategy);

	Q_INVOKABLE void		clearData();

private:
	QString		m_oldName;
	QString		m_oldValue;
	QVariantMap m_oldMetadata;

	QString		m_newName;
	QString		m_newValue;
	QVariantMap m_newMetadata;

	QString		m_importName;
	QString		m_format;
	QString		m_file;
	QString		m_mergeStrategy;

};

#endif // DATACONTAINER_HPP
