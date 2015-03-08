#include "datacontainer.hpp"

QString DataContainer::oldName() const
{
	return m_oldName;
}

void DataContainer::setOldName(const QString &oldName)
{
	m_oldName = oldName;
}

QString DataContainer::oldValue() const
{
	return m_oldValue;
}

void DataContainer::setOldValue(const QString &oldValue)
{
	m_oldValue = oldValue;
}

QVariantMap DataContainer::oldMetadata() const
{
	return m_oldMetadata;
}

void DataContainer::setOldMetadata(TreeViewModel *oldMetadata)
{
	//convert TreeViewModel to QVariantMap
	QVariantMap oldMDMap;

	if(oldMetadata)
	{
		foreach (ConfigNodePtr node, oldMetadata->model())
		{
			oldMDMap.insert(node->getName(), node->getValue());
		}

	}

	m_oldMetadata = oldMDMap;
}

QString DataContainer::newName() const
{
	return m_newName;
}

void DataContainer::setNewName(const QString &newName)
{
	m_newName = newName;
}

QString DataContainer::newValue() const
{
	return m_newValue;
}

void DataContainer::setNewValue(const QString &newValue)
{
	m_newValue = newValue;
}

QVariantMap DataContainer::newMetadata() const
{
	return m_newMetadata;
}

void DataContainer::setNewMetadata(const QVariantMap &newMetadata)
{
	m_newMetadata = newMetadata;
}

QString DataContainer::importName() const
{
	return m_importName;
}

void DataContainer::setImportName(const QString &importName)
{
	m_importName = importName;
}

QString DataContainer::format() const
{
	return m_format;
}

void DataContainer::setFormat(const QString &format)
{
	m_format = format;
}

QString DataContainer::file() const
{
	return m_file;
}

void DataContainer::setFile(const QString &file)
{
	m_file = file;
}

QVariantList DataContainer::mergeStrategies() const
{
	return m_mergeStrategies;
}

void DataContainer::setMergeStrategies(const QVariantList &mergeStrategies)
{
	m_mergeStrategies = mergeStrategies;
}

void DataContainer::clearData()
{
	m_oldName = "";
	m_oldValue = "";
	m_oldMetadata = QVariantMap();

	m_newName = "";
	m_newValue = "";
	m_newMetadata = QVariantMap();

	m_importName = "";
	m_format = "";
	m_file = "";
	m_mergeStrategies = QVariantList();
}
