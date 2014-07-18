#ifndef CONFIGNODE_H
#define CONFIGNODE_H

#include <QObject>
#include <QVariant>
#include <kdb.hpp>
#include <keyio.hpp>

#include "treeviewmodel.hpp"

class TreeViewModel;

class ConfigNode : public QObject
{
	Q_OBJECT

public:
	// TODO: why not pass the KeySet*
	explicit ConfigNode(const QString& name, const QString& path);
	/// Needed by Qt
	ConfigNode(const ConfigNode& other);
	/// Needed by Qt
	ConfigNode();
	~ConfigNode();

	// TODO: documentation!
	int                     getChildCount() const;
	QString                 getName() const;
	QString                 getPath() const;
	QVariant                getValue() const;
	void                    setName(const QString& name);
	void                    setValue(const QVariant& value);
	void                    appendChild(ConfigNode* node);
	bool                    hasChild(const QString& name) const;
	TreeViewModel*          getChildren();
	TreeViewModel*          getMetaValue();
	bool                    childrenHaveNoChildren() const;
	ConfigNode*             getChildByName(QString& name);
	Q_INVOKABLE ConfigNode* getChildByIndex(int index);

private:
	// TODO: not needed if we hold the Key
	QString m_name;
	QString m_path;
	QVariant m_value;

	// that is the only part we need:
	kdb::Key m_key;
	TreeViewModel* m_children;

signals:
	void nameChanged();
};

Q_DECLARE_METATYPE(ConfigNode)

#endif // CONFIGNODE_H
