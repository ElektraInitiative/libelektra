#ifndef DATACONTAINER_HPP
#define DATACONTAINER_HPP

#include <QObject>
#include "treeviewmodel.hpp"

class DataContainer : public QObject
{
	Q_OBJECT

public:
	explicit DataContainer(QObject *parentContainer = 0) : QObject(parentContainer) {}
	DataContainer(const DataContainer &otherContainer) : QObject() {Q_UNUSED(otherContainer)}

	Q_INVOKABLE QString			oldName() const;
	Q_INVOKABLE void			setOldName(const QString &name);

	Q_INVOKABLE QString			oldValue() const;
	Q_INVOKABLE void			setOldValue(const QString &value);

	Q_INVOKABLE QVariantMap		oldMetadata() const;
	Q_INVOKABLE void			setOldMetadata(TreeViewModel *metadata);

	Q_INVOKABLE QString			newName() const;
	Q_INVOKABLE void			setNewName(const QString &name);

	Q_INVOKABLE QString			newValue() const;
	Q_INVOKABLE void			setNewValue(const QString &value);

	Q_INVOKABLE QVariantMap		newMetadata() const;
	Q_INVOKABLE void			setNewMetadata(const QVariantMap &metadata);

	Q_INVOKABLE QString			importName() const;
	Q_INVOKABLE void			setImportName(const QString &name);

	Q_INVOKABLE QString			format() const;
	Q_INVOKABLE void			setFormat(const QString &form);

	Q_INVOKABLE QString			file() const;
	Q_INVOKABLE void			setFile(const QString &fil);

	Q_INVOKABLE QVariantList	mergeStrategies() const;
	Q_INVOKABLE void			setMergeStrategies(const QVariantList &strategies);

	Q_INVOKABLE void			clearData();

private:
	QString			m_oldName;
	QString			m_oldValue;
	QVariantMap		m_oldMetadata;

	QString			m_newName;
	QString			m_newValue;
	QVariantMap		m_newMetadata;

	QString			m_importName;
	QString			m_format;
	QString			m_file;
	QVariantList	m_mergeStrategies;

};

#endif // DATACONTAINER_HPP
