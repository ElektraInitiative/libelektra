/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef DATACONTAINER_HPP
#define DATACONTAINER_HPP

#include "./treeviewmodel.hpp"
#include <QObject>

/**
 * @brief The DataContainer class. It encapsulates various properties to avoid long method signatures.
 */

class DataContainer : public QObject
{
	Q_OBJECT

public:
	/**
	 * @brief The default constructor.
	 * @param parentContainer An optional parent object.
	 */
	explicit DataContainer (QObject * parentContainer = nullptr) : QObject (parentContainer)
	{
	}

	/**
	 * @brief The mandatory copy construcor-
	 */
	DataContainer (const DataContainer & otherContainer) : QObject ()
	{
		Q_UNUSED (otherContainer)
	}

	/**
	 * @brief The old name of a ConfigNode. Used when creating EditKeyCommands.
	 * @return The old name of a ConfigNode.
	 */
	Q_INVOKABLE QString oldName () const;

	/**
	 * @brief Sets the old name of a ConfigNode. Used when creating EditKeyCommands.
	 * @param name The old name of a ConfigNode.
	 */
	Q_INVOKABLE void setOldName (const QString & name);

	/**
	 * @brief The old value of a ConfigNode. Used when creating EditKeyCommands.
	 * @return The old value of a ConfigNode.
	 */
	Q_INVOKABLE QString oldValue () const;

	/**
	 * @brief Sets the old value of a ConfigNode. Used when creating EditKeyCommands.
	 * @param value The old value of a ConfigNode.
	 */
	Q_INVOKABLE void setOldValue (const QString & value);

	/**
	 * @brief The old metadata of a ConfigNode. Used when creating EditKeyCommands.
	 * @return The old metadata of a ConfigNode.
	 */
	Q_INVOKABLE QVariantMap oldMetadata () const;

	/**
	 * @brief Sets the old metadata of a ConfigNode. Used when creating EditKeyCommands.
	 * @param metadata The old metadata of a ConfigNode.
	 */
	Q_INVOKABLE void setOldMetadata (TreeViewModel * metadata);

	/**
	 * @brief The new name of a ConfigNode. Used when creating EditKeyCommands and NewKeyCommands.
	 * @return The new name of a ConfigNode.
	 */
	Q_INVOKABLE QString newName () const;

	/**
	 * @brief Sets the new name of a ConfigNode. Used when creating EditKeyCommands and NewKeyCommands.
	 * @param name The new name of a ConfigNode.
	 */
	Q_INVOKABLE void setNewName (const QString & name);

	/**
	 * @brief The new value of a ConfigNode. Used when creating EditKeyCommands and NewKeyCommands.
	 * @param name The new value of a ConfigNode.
	 */
	Q_INVOKABLE QString newValue () const;

	/**
	 * @brief Sets the new value of a ConfigNode. Used when creating EditKeyCommands and NewKeyCommands.
	 * @param name The new value of a ConfigNode.
	 */
	Q_INVOKABLE void setNewValue (const QString & value);

	/**
	 * @brief The new metadata of a ConfigNode. Used when creating EditKeyCommands and NewKeyCommands.
	 * @param name The new metadata of a ConfigNode.
	 */
	Q_INVOKABLE QVariantMap newMetadata () const;

	/**
	 * @brief Sets the new metadata of a ConfigNode. Used when creating EditKeyCommands and NewKeyCommands.
	 * @param name The new metadata of a ConfigNode.
	 */
	Q_INVOKABLE void setNewMetadata (const QVariantMap & metadata);

	/**
	 * @brief The name of the ConfigNode that will be the root ConfigNode when importing a configuration from file. Used when creating
	 * ImportConfigurationCommands.
	 * @return The name of the ConfigNode that will be the root ConfigNode when importing a configuration from file.
	 */
	Q_INVOKABLE QString importName () const;

	/**
	 * @brief Sets the name of the ConfigNode that will be the root ConfigNode when importing a configuration from file. Used when
	 * creating ImportConfigurationCommands.
	 * @param name The name of the ConfigNode that will be the root ConfigNode when importing a configuration from file.
	 */
	Q_INVOKABLE void setImportName (const QString & name);

	/**
	 * @brief The format of the file that contains the configuration to import. Used when creating ImportConfigurationCommands.
	 * @return The format of the file that contains the configuration to import.
	 */
	Q_INVOKABLE QString format () const;

	/**
	 * @brief Sets the format of the file that contains the configuration to import. Used when creating ImportConfigurationCommands.
	 * @param form The format of the file that contains the configuration to import.
	 */
	Q_INVOKABLE void setFormat (const QString & form);

	/**
	 * @brief The system path of the file that contains the configuration to import. Used when creating ImportConfigurationCommands.
	 * @return The system path of the file that contains the configuration to import.
	 */
	Q_INVOKABLE QString file () const;

	/**
	 * @brief Sets the system path of the file that contains the configuration to import. Used when creating
	 * ImportConfigurationCommands.
	 * @param fil The system path of the file that contains the configuration to import.
	 */
	Q_INVOKABLE void setFile (const QString & fil);

	/**
	 * @brief The merge strategies used when importing a configuration from file. Used when creating ImportConfigurationCommands.
	 * @return The merge strategies used when importing a configuration from file.
	 */
	Q_INVOKABLE QVariantList mergeStrategies () const;

	/**
	 * @brief Sets the merge strategies used when importing a configuration from file. Used when creating ImportConfigurationCommands.
	 * @param strategies The merge strategies used when importing a configuration from file.
	 */
	Q_INVOKABLE void setMergeStrategies (const QVariantList & strategies);

	/**
	 * @brief Sets all properties to empty values.
	 */
	Q_INVOKABLE void clearData ();

private:
	QString m_oldName;
	QString m_oldValue;
	QVariantMap m_oldMetadata;

	QString m_newName;
	QString m_newValue;
	QVariantMap m_newMetadata;

	QString m_importName;
	QString m_format;
	QString m_file;
	QVariantList m_mergeStrategies;
};

#endif // DATACONTAINER_HPP
