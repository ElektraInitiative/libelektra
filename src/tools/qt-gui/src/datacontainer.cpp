/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./datacontainer.hpp"

QString DataContainer::oldName () const
{
	return m_oldName;
}

void DataContainer::setOldName (const QString & name)
{
	m_oldName = name;
}

QString DataContainer::oldValue () const
{
	return m_oldValue;
}

void DataContainer::setOldValue (const QString & value)
{
	m_oldValue = value;
}

QVariantMap DataContainer::oldMetadata () const
{
	return m_oldMetadata;
}

void DataContainer::setOldMetadata (TreeViewModel * metadata)
{
	// convert TreeViewModel to QVariantMap
	QVariantMap oldMDMap;

	if (metadata)
	{
		foreach (ConfigNodePtr node, metadata->model ())
		{
			oldMDMap.insert (node->getName (), node->getValue ());
		}
	}

	m_oldMetadata = oldMDMap;
}

QString DataContainer::newName () const
{
	return m_newName;
}

void DataContainer::setNewName (const QString & name)
{
	m_newName = name;
}

QString DataContainer::newValue () const
{
	return m_newValue;
}

void DataContainer::setNewValue (const QString & value)
{
	m_newValue = value;
}

QVariantMap DataContainer::newMetadata () const
{
	return m_newMetadata;
}

void DataContainer::setNewMetadata (const QVariantMap & metadata)
{
	m_newMetadata = metadata;
}

QString DataContainer::importName () const
{
	return m_importName;
}

void DataContainer::setImportName (const QString & name)
{
	m_importName = name;
}

QString DataContainer::format () const
{
	return m_format;
}

void DataContainer::setFormat (const QString & form)
{
	m_format = form;
}

QString DataContainer::file () const
{
	return m_file;
}

void DataContainer::setFile (const QString & fil)
{
	m_file = fil;
}

QVariantList DataContainer::mergeStrategies () const
{
	return m_mergeStrategies;
}

void DataContainer::setMergeStrategies (const QVariantList & strategies)
{
	m_mergeStrategies = strategies;
}

void DataContainer::clearData ()
{
	m_oldName = "";
	m_oldValue = "";
	m_oldMetadata = QVariantMap ();

	m_newName = "";
	m_newValue = "";
	m_newMetadata = QVariantMap ();

	m_importName = "";
	m_format = "";
	m_file = "";
	m_mergeStrategies = QVariantList ();
}
