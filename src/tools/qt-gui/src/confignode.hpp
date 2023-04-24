/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CONFIGNODE_H
#define CONFIGNODE_H

#include <QObject>
#include <QSharedPointer>
#include <QVariant>

#include <cassert>
#include <kdb.hpp>
#include <keyio.hpp>

#include "./printvisitor.hpp"

class TreeViewModel;
class PrintVisitor;

/**
 * @brief The ConfigNode class
 */
class ConfigNode : public QObject
{
	Q_OBJECT

public:
	/**
	 * @brief ConfigNode
	 * @param name
	 * @param path
	 * @param key
	 * @param parentModel
	 */
	explicit ConfigNode (QString name, QString path, const kdb::Key & key, TreeViewModel * parentModel);
	/// Needed by Qt. This copy constructor is supposed to create a DEEP COPY.
	ConfigNode (const ConfigNode & other);
	/// Needed by Qt/QSharedPtr
	ConfigNode ();
	~ConfigNode ();

	/**
	 * @brief dontDelete
	 */
	void dontDelete ()
	{
	}

	/**
	 * @brief Returns the number of children of this ConfigNode.
	 *
	 * @return The number of children of this ConfigNode.
	 */
	int getChildCount () const;

	/**
	 * @brief Returns the name of this ConfigNode.
	 *
	 * @return The name of this ConfigNode.
	 */
	QString getName () const;

	/**
	 * @brief Returns the path of this ConfigNode.
	 *
	 * @return The path of this ConfigNode.
	 */
	QString getPath () const;

	/**
	 * @brief Returns the value of this ConfigNode.
	 *
	 * @return The value of this ConfigNode.
	 */
	QVariant getValue () const;

	/**
	 * @brief Rename this ConfigNode.
	 *
	 * @param name The new name for this ConfigNode.
	 */
	void setName (const QString & name);

	/**
	 * @brief Change the value of this ConfigNode.
	 *
	 * @param value The new value for this ConfigNode.
	 */
	void setValue (const QVariant & value);

	/**
	 * @brief Append a new child to this ConfigNode.
	 *
	 * @param node The new child of this ConfigNode.
	 */
	void appendChild (QSharedPointer<ConfigNode> node);

	/**
	 * @brief Returns if this ConfigNode has a child with a certain name.
	 *
	 * @param name The name of the child node.
	 *
	 * @return True if this node has a child with a certain name.
	 */
	bool hasChild (const QString & name) const;

	/**
	 * @brief Get the children of this ConfigNode.
	 *
	 * @return The children of this ConfigNode as model.
	 */
	TreeViewModel * getChildren () const;

	/**
	 * @brief Get the metakeys of this ConfigNode.
	 *
	 * @return The metakeys of this ConfigNode as model.
	 */
	TreeViewModel * getMetaKeys () const;

	/**
	 * @brief Returns if the children of this ConfigNode have any children themselves.
	 *
	 * @return True if no child of this ConfigNode has any children.
	 */
	bool childrenHaveNoChildren () const;

	/**
	 * @brief Returns a child with a certain name.
	 *
	 * @param name The name of the child which is looked for.
	 *
	 * @return The child with the given name if it is a child of this ConfigNode.
	 */
	QSharedPointer<ConfigNode> getChildByName (QString & name) const;

	/**
	 * @brief Returns a child on a given index.
	 *
	 * @param index The index of the wanted child.
	 *
	 * @return The child on the given index.
	 */
	Q_INVOKABLE QSharedPointer<ConfigNode> getChildByIndex (int index) const;

	/**
	 * @brief Change the path of this ConfigNode.
	 *
	 * @param path The new path of this ConfigNode.
	 */
	void setPath (const QString & path);

	/**
	 * @brief Change or add a metakey of this ConfigNode. This method is used if this ConfigNode is a metakey.
	 *
	 * @param name The name of the metakey.
	 * @param value The value of the metakey.
	 */
	void setMeta (const QString & name, const QVariant & value);

	/**
	 * @brief Change or add more than one metakeys of this ConfigNode. This method is used if this ConfigNode is a metakey.
	 *
	 * @param metaData The data consists of metanames and metavalues respectively.
	 */
	Q_INVOKABLE void setMeta (const QVariantMap & metaData);

	/**
	 * @brief Delete a metakey of this ConfigNode. This method is used if this ConfigNode is a metakey.
	 *
	 * @param name The name of the metakey that is supposed to be deleted.
	 */
	Q_INVOKABLE void deleteMeta (const QString & name);

	/**
	 * @brief This method accepts a visitor to support the Vistor Pattern.
	 *
	 * @param visitor The Visitor that visits this ConfigNode.
	 */
	void accept (Visitor & visitor);

	/**
	 * @brief Get the underlying Key of this ConfigNode.
	 *
	 * @return The underlying Key of this ConfigNode.
	 */
	kdb::Key getKey () const;

	/**
	 * @brief Set the underlying Key of this ConfigNode.
	 *
	 * @param key The new Key of this ConfigNode.
	 */
	void setKey (kdb::Key key);

	/**
	 * @brief Change the name of the underlying Key of this ConfigNode.
	 *
	 * @param name The new name of the underlying Key of this ConfigNode.
	 */
	void setKeyName (const QString & name);

	/**
	 * @brief Returns the index of a child ConfigNode, based on its name; if there is no child with this
	 * name, the return index is -1.
	 *
	 * @param name The name of this ConfigNode.
	 *
	 * @return The index of this ConfigNode.
	 */
	int getChildIndexByName (const QString & name);

	/**
	 * @brief Returns a pointer to the TreeViewModel this ConfigNode is in.
	 *
	 * @return A pointer to the TreeViewModel this ConfigNode is in.
	 */
	TreeViewModel * getParentModel ();

	/**
	 * @brief Sets a pointer to the TreeViewModel this ConfigNode is in.
	 *
	 * @param parentModel The TreeViewModel this ConfigNode is in.
	 */
	void setParentModel (TreeViewModel * parentModel);

	/**
	 * @brief Returns if this ConfigNode is expanded.
	 *
	 * @return True if this ConfigNode is expanded.
	 */
	bool isExpanded () const;

	bool isDirty () const;
	void setIsDirty (bool dirty);
	void updateNode (kdb::Key key);

	/**
	 * @return true if this is ConfigNode is the root of a namespace
	 */
	bool isNamespaceRoot () const;

private:
	QString m_name;
	QString m_path;
	QVariant m_value;

	kdb::Key m_key;
	TreeViewModel * m_children;
	TreeViewModel * m_metaData;
	TreeViewModel * m_parentModel;

	bool m_isExpanded;
	bool m_isDirty;

	/**
	 * @brief Populates the TreeViewModel which holds the metakeys of this ConfigNode.
	 */
	void populateMetaModel ();
	void setValue ();

signals:
	/**
	 * @brief showMessage
	 * @param title
	 * @param text
	 * @param detailedText
	 */
	void showMessage (QString title, QString text, QString detailedText);

public slots:
	/**
	 * @brief setIsExpanded
	 * @param value
	 */
	void setIsExpanded (bool value);
};

Q_DECLARE_METATYPE (ConfigNode)

typedef QSharedPointer<ConfigNode> ConfigNodePtr;

#endif // CONFIGNODE_H
