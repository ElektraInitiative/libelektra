#ifndef DATACONTAINER_HPP
#define DATACONTAINER_HPP

#include <QObject>
#include "treeviewmodel.hpp"

/**
 * @brief The DataContainer class
 */

class DataContainer : public QObject
{
	Q_OBJECT

public:
	/**
	 * @brief DataContainer
	 * @param parentContainer
	 */
	explicit DataContainer(QObject *parentContainer = 0) : QObject(parentContainer) {}

	/**
	 * @brief DataContainer
	 * @param otherContainer
	 */
	DataContainer(const DataContainer &otherContainer) : QObject() {Q_UNUSED(otherContainer)}

	/**
	  * @brief oldName
	  * @return
	  */
	Q_INVOKABLE QString			oldName() const;

	/**
	  * @brief setOldName
	  * @param name
	  */
	Q_INVOKABLE void			setOldName(const QString &name);

	/**
	  * @brief oldValue
	  * @return
	  */
	Q_INVOKABLE QString			oldValue() const;

	/**
	  * @brief setOldValue
	  * @param value
	  */
	Q_INVOKABLE void			setOldValue(const QString &value);

	/**
	  * @brief oldMetadata
	  * @return
	  */
	Q_INVOKABLE QVariantMap		oldMetadata() const;

	/**
	  * @brief setOldMetadata
	  * @param metadata
	  */
	Q_INVOKABLE void			setOldMetadata(TreeViewModel *metadata);

	/**
	  * @brief newName
	  * @return
	  */
	Q_INVOKABLE QString			newName() const;

	/**
	  * @brief setNewName
	  * @param name
	  */
	Q_INVOKABLE void			setNewName(const QString &name);

	/**
	  * @brief newValue
	  * @return
	  */
	Q_INVOKABLE QString			newValue() const;

	/**
	  * @brief setNewValue
	  * @param value
	  */
	Q_INVOKABLE void			setNewValue(const QString &value);

	/**
	  * @brief newMetadata
	  * @return
	  */
	Q_INVOKABLE QVariantMap		newMetadata() const;

	/**
	  * @brief setNewMetadata
	  * @param metadata
	  */
	Q_INVOKABLE void			setNewMetadata(const QVariantMap &metadata);

	/**
	  * @brief importName
	  * @return
	  */
	Q_INVOKABLE QString			importName() const;

	/**
	  * @brief setImportName
	  * @param name
	  */
	Q_INVOKABLE void			setImportName(const QString &name);

	/**
	  * @brief format
	  * @return
	  */
	Q_INVOKABLE QString			format() const;

	/**
	  * @brief setFormat
	  * @param form
	  */
	Q_INVOKABLE void			setFormat(const QString &form);

	/**
	  * @brief file
	  * @return
	  */
	Q_INVOKABLE QString			file() const;

	/**
	  * @brief setFile
	  * @param fil
	  */
	Q_INVOKABLE void			setFile(const QString &fil);

	/**
	  * @brief mergeStrategies
	  * @return
	  */
	Q_INVOKABLE QVariantList	mergeStrategies() const;

	/**
	  * @brief setMergeStrategies
	  * @param strategies
	  */
	Q_INVOKABLE void			setMergeStrategies(const QVariantList &strategies);

	/**
	  * @brief clearData
	  */
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
