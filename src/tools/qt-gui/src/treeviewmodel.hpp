#ifndef TREEVIEWMODEL_H
#define TREEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QDebug>
#include <QtQml>
#include <kdb.hpp>
#include <keyio.hpp>
#include <backend.hpp>

#include "confignode.hpp"
#include "printvisitor.hpp"
#include "keysetvisitor.hpp"

class Visitor;

class TreeViewModel : public QAbstractListModel
{

	Q_OBJECT

public:

	/**
	 * @brief The TreeViewModelRoles enum
	 */
	enum TreeViewModelRoles
	{
		NameRole = Qt::UserRole + 1, ///< The role QML can access the name of a ConfigNode at a specified index.
		PathRole, ///< The role QML can access the path of a ConfigNode at a specified index.
		ValueRole, ///< The role QML can access the value of a ConfigNode at a specified index.
		ChildCountRole, ///< The role QML can access the number of children of a ConfigNode at a specified index.
		ChildrenRole, ///< The role QML can access the children model of a ConfigNode at a specified index.
		ChildrenHaveNoChildrenRole, ///< The role QML can access if children of a ConfigNode at a specified index do have children on their own.
		MetaValueRole, ///< The role QML can access the meta model of a ConfigNode at a specified index.
		NodeRole, ///< The role QML can retrieve the ConfigNode at a specified index.
		ParentModelRole, ///< The role QML can retrieve a pointer to the ParentModel of a ConfigNode.
		IndexRole, ///< The role QML can retrieve the index of a ConfigNode.
		IsNullRole, ///< The role QML can retrieve if the key of this node is null.
		IsExpandedRole ///< The role QML can retrieve if a ConfigNode is expanded.
	};

	explicit TreeViewModel(QObject* parent =  0);

	// Needed for Qt
	TreeViewModel(TreeViewModel const& other);

	// @return the underlying model
	QList<ConfigNodePtr>& model()
	{
		return m_model;
	}

	//mandatory methods inherited from QAbstractItemModel
	Q_INVOKABLE int             rowCount(const QModelIndex& parent = QModelIndex()) const;
	QVariant                    data(const QModelIndex& index, int role = Qt::DisplayRole) const;
	bool                        setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
	Q_INVOKABLE bool            insertRow(int row, const QModelIndex& parent = QModelIndex());
	Q_INVOKABLE bool            removeRow(int row, const QModelIndex& parent = QModelIndex());
	Qt::ItemFlags               flags(const QModelIndex& index) const;

	/**
	 * @brief Populates this TreeViewModel with the *current* keyset.
	 */

	Q_INVOKABLE void            populateModel();

	/**
	 * @brief Populates this TreeViewModel with *an actualised* keyset (calls @see #kdbGet and @see #kdbSet before populating).
	 */

	Q_INVOKABLE void			repopulateModel();

	/**
	 * @brief The method that actually populates this TreeViewModel.
	 *
	 * @param node The ConfigNode that is supposed to find its place in the hierarchy.
	 * @param keys The path of the ConfigNode that is supposed to find its place in the hierarchy, splitted up into a QStringList.
	 * @param path The current path of the ConfigNode.
	 * @param key The Key that the ConfigNode holds. If it is no leaf node, the Key is NULL.
	 */
	void                        sink(ConfigNodePtr node, QStringList keys, QString path, kdb::Key key);

	void                        accept(Visitor& visitor);

	/**
	 * @brief Get the roles of a ConfigNode at the specifies index. Needed to access roles from outside a delegate in QML.
	 *
	 * @param idx The index of the ConfigNode.
	 *
	 * @return A map of the roles of the ConfigNode at the specified index.
	 */
	Q_INVOKABLE QVariantMap     get(int idx) const;

	/**
	  * @brief Find a search term in the model.
	  *
	  * @param term The search term of interest.
	  *
	  * @return A model which includes all ConfigNodes that have the search term in their name or value.
	  */
	Q_INVOKABLE QVariant        find(const QString& term);

	/**
	 * @brief Inserts a new ConfigNode at a specified index into this TreeViewModel. This method is used if this TreeViewModel is holding meta keys.
	 *
	 * @param row The index the new ConfigNode is supposed to be inserted at.
	 * @param node The ConfigNode that is supposed to be inserted.
	 */
	void                        insertMetaRow(int row, kdb::Key key, const QString &name);

	/**
	 * @brief Inserts a new ConfigNode at a specified index into this TreeViewModel. This method is used if this TreeViewModel is holding non metakey ConfigNodes.
	 *
	 * @param row The index the new ConfigNode is supposed to be inserted at.
	 * @param node The ConfigNode that is supposed to be inserted.
	 */
	void                        insertRow(int row, ConfigNodePtr node);

	/**
	 * @brief Looks for valid ConfigNodes, adds them to a KeySet and repopulates this TreeViewModel based on the KeySet.
	 *
	 */
	Q_INVOKABLE void            synchronize();

	/**
	 * @brief Creates a new ConfigNode and puts it into its place in the hierarchy.
	 *
	 * @param path The path (full keyname) of the ConfigNode.
	 * @param value The value of the ConfigNode.
	 * @param metaData The metada of the ConfigNode.
	 */
	Q_INVOKABLE kdb::Key		createNewKey(const QString& path, const QString& value, const QVariantMap metaData);

	/**
	 * @brief Appends a ConfigNode to this TreeViewModel. At the time of insertion the index of the ConfigNode will be the largest in this model.
	 *
	 * @param node The ConfigNode that is appended to this TreeViewModel.
	 */
	void                        append(ConfigNodePtr node);

	/**
	 * @brief A version of the mandatory setData method that can be called from QML without a QModelIndex. It creates a QModelIndex and calls the mandatory setData method.
	 *
	 * @param index The index of the ConfigNode inside this TreeViewModel that is supposed to be manipulated.
	 * @param value Holds the data to manipulate the ConfigNode.
	 * @param role Holds the role name that determines the type of the manipulation.
	 */
	Q_INVOKABLE void            setData(int index, const QVariant& value, const QString& role);

	/**
	 * @brief Returns the index of a ConfigNode in this TreeViewModel based in the ConfigNode's name.
	 *
	 * @param name The name of the ConfigNode.
	 *
	 * @return The index of the ConfigNode, -1 if a ConfigNode with this name is not present in this TreeViewModel.
	 */
	Q_INVOKABLE int             getIndexByName(const QString& name) const;

	/**
	 * @brief Export the configuration below a ConfigNode to a file on the harddisk.
	 *
	 * @param node The ConfigNode that is the root node of the exported configuration.
	 * @param format Specifies the file format of the exported file.
	 * @param file The path on the harddisk where the exported file is written to.
	 */
	Q_INVOKABLE void            exportConfiguration(TreeViewModel* model, int index, QString format, QString file);

	/**
	 * @brief Import a configuration from a file on the harddrive into the current configuration.
	 *
	 * @param name The name of the ConfigNode the configuration is imported to.
	 * @param file The path of the file on the harddisk.
	 * @param format The format of the file on the harddisk.
	 * @param mergeStrategy The mergeStrategy in case of conflict.
	 */
	Q_INVOKABLE void            importConfiguration(const QString& name, const QString& file, QString& format, const QString& mergeStrategy);

	/**
	 * @brief Sets a new KeySet.
	 *
	 * @param set The new KeySet.
	 */
	void                        setKeySet(kdb::KeySet set);

	/**
	 * @brief Gets this model's current KeySet.
	 *
	 * @return This model's current KeySet.
	 */
	kdb::KeySet					getKeySet();

	/**
	 * @brief Stores the current state of the configuration in the KeySet.
	 */
	void                        collectCurrentKeySet();

	/**
	 * @brief Clears this model if it holds metakeys.
	 */
	void                        clearMetaModel();

	/**
	 * @brief Unmounts a backend.
	 *
	 * @param backendName The name of the backend that is unmounted.
	 */
	Q_INVOKABLE void            unMountBackend(QString backendName);

	/**
	 * @brief Recreates the model. Needed to update the QML view.
	 */
	Q_INVOKABLE void            refresh();

	/**
	 * @brief The number of ConfigNodes in this model.
	 *
	 * @return The number of ConfigNodes in this model.
	 */
	Q_INVOKABLE int             count() const;

	/**
	 * @brief Returns the correct name for a new Array Entry.
	 *
	 * @return The correct name for a new Array Entry.
	 */
	Q_INVOKABLE QString			getCurrentArrayNo() const;

	/**
	 * @brief Renames all Array Entries in this model to prevent holes.
	 */
	void						refreshArrayNumbers();


	/**
	 * @brief Returns a list of the current mounted backends.
	 *
	 * @return A list of the current mounted backends.
	 */
	Q_INVOKABLE QStringList     mountedBackends();

private:

	/**
	 * @brief A private method that is called from the public @see #find method. It performs the actual search.
	 *
	 * @param node The ConfigNode that is to be searched next.
	 * @param searchResults The TreeViewModel that holds the search results.
	 * @param term The term that is searched for.
	 */
	void                        find(ConfigNodePtr node, TreeViewModel* searchResults, const QString term);

	QList<ConfigNodePtr>        m_model;
	kdb::Key                    m_metaModelParent;
	kdb::KDB                    m_kdb;
	kdb::KeySet                 m_keySet;

protected:
	QHash<int, QByteArray>      roleNames() const;

signals:
	void						showMessage(QString title, QString text, QString detailedText) const;
	void						updateProgress(int value) const;
	void						expandNode(bool);
	void						finished() const;

public slots:
	void						showConfigNodeMessage(QString title, QString text, QString detailedText);

};

Q_DECLARE_METATYPE(TreeViewModel)

#endif // TREEVIEWMODEL_H
