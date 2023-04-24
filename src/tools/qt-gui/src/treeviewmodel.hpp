/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef TREEVIEWMODEL_H
#define TREEVIEWMODEL_H

#include <QAbstractListModel>
#include <QDebug>
#include <QList>
#include <QtQml>
#include <backend.hpp>
#include <kdb.hpp>
#include <keyio.hpp>

#include <merging/automergeconfiguration.hpp>
#include <merging/mergingkdb.hpp>

#include "./confignode.hpp"
#include "./findvisitor.hpp"
#include "./keysetvisitor.hpp"
#include "./printvisitor.hpp"

class Visitor;

/**
 * @brief The TreeViewModel class. It holds ConfigNodes.
 */

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
		PathRole,		     ///< The role QML can access the path of a ConfigNode at a specified index.
		ValueRole,		     ///< The role QML can access the value of a ConfigNode at a specified index.
		ChildCountRole,		     ///< The role QML can access the number of children of a ConfigNode at a specified index.
		ChildrenRole,		     ///< The role QML can access the children model of a ConfigNode at a specified index.
		ChildrenHaveNoChildrenRole,  ///< The role QML can access if children of a ConfigNode at a specified index do have children
		/// on their own.
		MetaValueRole,	     ///< The role QML can access the meta model of a ConfigNode at a specified index.
		NodeRole,	     ///< The role QML can retrieve the ConfigNode at a specified index.
		ParentModelRole,     ///< The role QML can retrieve a pointer to the ParentModel of a ConfigNode.
		IndexRole,	     ///< The role QML can retrieve the index of a ConfigNode.
		IsNullRole,	     ///< The role QML can retrieve if the key of this node is null.
		IsExpandedRole,	     ///< The role QML can retrieve if a ConfigNode is expanded.
		IsNamespaceRootRole, ///< The role QML can use to check whether a ConfigNode is a namespace root.
	};

	/**
	 * @brief The default constructor.
	 * @param parentModel An optional parent.
	 */
	explicit TreeViewModel (QObject * parentModel = nullptr);

	/**
	 * @brief Constructor for root node.
	 * @param parentModel An optional parent.
	 */
	explicit TreeViewModel (kdb::tools::merging::MergingKDB * kdb, QObject * parentModel = nullptr);

	/**
	 * @brief The mandatory copy constructor.
	 * @param The TreeViewModel that is copied.
	 */
	TreeViewModel (TreeViewModel const & other);

	/**
	 * @brief Returns the QList that holds the ConfigNodes.
	 * @return The QList that holds the ConfigNodes.
	 */
	QList<ConfigNodePtr> & model ()
	{
		return m_model;
	}

	// mandatory methods inherited from QAbstractItemModel

	/**
	 * @copydoc QAbstractListModel::rowCount()
	 */
	Q_INVOKABLE int rowCount (const QModelIndex & parentIndex = QModelIndex ()) const override;

	/**
	 * @copydoc QAbstractListModel::data()
	 */
	QVariant data (const QModelIndex & idx, int role = Qt::DisplayRole) const override;

	/**
	 * @copydoc QAbstractListModel::setData()
	 */
	bool setData (const QModelIndex & idx, const QVariant & modelData, int role = Qt::EditRole) override;

	/**
	 * \copydoc QAbstractListModel::insertRow()
	 */
	Q_INVOKABLE bool insertRow (int row, const QModelIndex & parentIndex = QModelIndex ());

	/**
	 * @copydoc QAbstractListModel::removeRow()
	 */
	Q_INVOKABLE bool removeRow (int row, const QModelIndex & parentIndex = QModelIndex ());

	/**
	 * @copydoc QAbstractListModel::flags()
	 */
	Qt::ItemFlags flags (const QModelIndex & idx) const override;

	/**
	 * @brief Populates this TreeViewModel with a keyset. The root keys (system, user and spec) will be recreated.
	 */
	Q_INVOKABLE void populateModel ();

	/**
	 * @brief Populates this TreeViewModel with a keyset. The root keys (system, user and spec) will be recreated.
	 * @param keySet The KeySet that holds the Key objects.
	 */
	void populateModel (kdb::KeySet const & keySet);

	/**
	 * @brief Populates this TreeViewModel with a keyset. The root keys (system, user and spec) will not be recreated.
	 * @param keySet The KeySet that holds the Key objects.
	 */
	void createNewNodes (kdb::KeySet keySet);

	/**
	 * @brief The recursive method that actually populates this TreeViewModel.
	 *
	 * @param node The ConfigNode that is supposed to find its place in the hierarchy.
	 * @param keys The path of the ConfigNode that is supposed to find its place in the hierarchy, splitted up into a QStringList.
	 * @param key The Key that the ConfigNode holds. If it is no leaf node, the Key is NULL.
	 */
	void sink (ConfigNodePtr node, QStringList keys, const kdb::Key & key);

	/**
	 * @brief The method thats accepts a Visitor object to support the Vistor Pattern.
	 * @param visitor The visitor that visits this TreeViewModel.
	 */
	void accept (Visitor & visitor);

	/**
	 * @brief Get the roles of a ConfigNode at the specifies index. Needed to access roles from outside a delegate in QML.
	 *
	 * @param idx The index of the ConfigNode.
	 *
	 * @return A map of the roles of the ConfigNode at the specified index.
	 */
	Q_INVOKABLE QVariantMap get (const int & idx) const;

	/**
	 * @brief Find a search term in the model.
	 *
	 * @param term The search term of interest.
	 *
	 * @return A model which includes all ConfigNodes that have the search term in their name, value or metakeys.
	 */
	Q_INVOKABLE QVariant find (const QString & term);

	/**
	 * @brief Inserts a new ConfigNode at a specified index into this TreeViewModel. This method is used if this TreeViewModel is
	 * holding metakeys.
	 * @param row The index the new ConfigNode is supposed to be inserted at.
	 * @param key The key that holds the metadata.
	 * @param name The name of the parent ConfigNode that holds the metadata.
	 */
	void insertMetaRow (int row, kdb::Key key, const QString & name);

	/**
	 * @brief Inserts a new ConfigNode at a specified index into this TreeViewModel. This method is used if this TreeViewModel is
	 * holding non metakey ConfigNodes.
	 * @param row The index the new ConfigNode is supposed to be inserted at.
	 * @param node The ConfigNode that is supposed to be inserted.
	 * @param addParent Determines if the parentModel of the ConfigNode should be set with this TreeViewModel.
	 */
	void insertRow (int row, ConfigNodePtr node, bool addParent = true);

	/**
	 * @brief Collects all current ConfigNodes, adds them to a KeySet, threewaymerges the KeySet with the permanent database
	 * and populates this TreeViewModel with the result.
	 */
	Q_INVOKABLE void synchronize ();

	/**
	 * @brief Creates a new ConfigNode and puts it into its place in the hierarchy.
	 *
	 * @param path The path (full keyname) of the ConfigNode.
	 * @param value The value of the ConfigNode.
	 * @param metaData The metada of the ConfigNode.
	 */
	Q_INVOKABLE kdb::Key createNewKey (const QString & path, const QString & value, const QVariantMap metaData);

	/**
	 * @brief Appends a ConfigNode to this TreeViewModel. At the time of insertion the index of the ConfigNode will be the largest in
	 * this model.
	 *
	 * @param node The ConfigNode that is appended to this TreeViewModel.
	 */
	void append (ConfigNodePtr node);

	/**
	 * @brief Returns the index of a ConfigNode in this TreeViewModel based in the ConfigNode's name.
	 *
	 * @param name The name of the ConfigNode.
	 *
	 * @return The index of the ConfigNode, -1 if a ConfigNode with this name is not present in this TreeViewModel.
	 */
	Q_INVOKABLE int getIndexByName (const QString & name) const;

	/**
	 * @brief Export the configuration below a ConfigNode to a file on the harddisk.
	 * @param parentModel The TreeViewModel that holds the ConfigNode.
	 * @param idx The index of the ConfigNode in the TreeViewModel.
	 * @param format Specifies the file format of the exported file.
	 * @param file The path on the harddisk where the exported file is written to.
	 */
	Q_INVOKABLE void exportConfiguration (TreeViewModel * parentModel, int idx, QString format, QString file);

	/**
	 * @brief Import a configuration from a file on the harddrive into the current configuration.
	 *
	 * @param name The name of the ConfigNode the configuration is imported to.
	 * @param file The path of the file on the harddisk.
	 * @param format The format of the file on the harddisk.
	 * @param mergeStrategies The mergeStrategies in case of conflict.
	 */
	Q_INVOKABLE void importConfiguration (const QString & name, const QString & file, QString & format,
					      const QVariantList & mergeStrategies);

	/**
	 * @brief Stores the current state of the configuration in the KeySet.
	 */
	kdb::KeySet collectCurrentKeySet ();

	/**
	 * @brief Clears this model if it holds metakeys.
	 */
	void clearMetaModel ();

	/**
	 * @brief Unmounts a backend.
	 *
	 * @param backendName The name of the backend that is unmounted.
	 */
	Q_INVOKABLE void unMountBackend (QString backendName);

	/**
	 * @brief Needed to update the QML view.
	 */
	Q_INVOKABLE void refresh ();

	/**
	 * @brief Returns the correct name for a new Array Entry.
	 *
	 * @return The correct name for a new Array Entry.
	 */
	Q_INVOKABLE QString getCurrentArrayNo () const;

	/**
	 * @brief Renames all Array Entries in this model to prevent holes.
	 */
	void refreshArrayNumbers ();

	/**
	 * @brief Returns a list of the currently mounted backends.
	 *
	 * @return A list of the current mounted backends.
	 */
	Q_INVOKABLE QStringList mountedBackends ();

	/**
	 * @brief getSplittedKeyname Splits a keyname into pieces. Delimiter is an unescaped slash ("/").
	 * @param key The key with the keyname of interest.
	 * @return A QStringList that holds the splitted keyname.
	 */
	QStringList getSplittedKeyname (const kdb::Key & key);

	/**
	 * @brief discardModel Allow the QML side to destroy this model, even if this model is owned by C++.
	 */
	Q_INVOKABLE void discardModel ();

private:
	QList<ConfigNodePtr> m_model;
	kdb::Key m_root;
	kdb::Key m_metaModelParent;
	kdb::tools::merging::MergingKDB * m_kdb;
	/**
	 * @brief Returns a MergeConflictStrategy object based on the name of the MergeConflictStrategy.
	 * @param mergeStrategy The name of the MergeConflictStrategy.
	 * @return The MergeConflictStrategy object based on the name of the MergeConflictStrategy.
	 */
	kdb::tools::merging::MergeConflictStrategy * getMergeStrategy (const QString & mergeStrategy);

	/**
	 * @brief Connect to system D-Bus
	 */
	void connectDBus ();

	/**
	 * @brief Extract the contents of an KDBException::what() function for usage in a TreeViewModel::showMessage() function.
	 * @param e The KDBException.
	 * @return A QMap with the contents of the KDBException.
	 */
	QMap<QString, QString> getErrorMessage (kdb::KDBException const & e);

protected:
	QHash<int, QByteArray> roleNames () const override;

signals: // Use "Error", "Warning" and "Information" as title to display the according icon
	/**
	 * @brief Triggers a messagedialog in the GUI.
	 * @param title The title of the messagedialog in the GUI.
	 * @param text The text of the messagedialog in the GUI.This is the text that will be initially shown to the user.
	 * @param detailedText The detailed text of the messagedialog in the GUI.The user will have to click on a button to access this
	 * text.
	 */
	void showMessage (QString title, QString text, QString detailedText) const;
	/**
	 * @brief Triggers the expanded property of a ConfigNode.
	 */
	void expandNode (bool);
	/**
	 * @brief Triggers the update of the treeview in the GUI.
	 */
	void updateIndicator () const;

public slots:

	/**
	 * @brief Displays a pop up window to the user
	 * @param title The title of the pop up window
	 * @param text The message of the pop up window
	 * @param detailedText A more detailed message to display
	 */
	void showConfigNodeMessage (QString title, QString text, QString detailedText);

	/**
	 * @brief Writes a change of the system config to std::out if in debug mode
	 *
	 * @param msg
	 */
	void configChanged (QString msg);
};

Q_DECLARE_METATYPE (TreeViewModel)

#endif // TREEVIEWMODEL_H
