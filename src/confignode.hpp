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

    explicit ConfigNode(const QString &name, const QString &path);
    ConfigNode(const ConfigNode &other);
    ConfigNode();
    ~ConfigNode();

    int getChildCount() const;
    QString getName() const;
    QString getPath() const;
    QVariantgetValue() const;
    voidsetName(const QString &name);
    voidsetValue(const QVariant &value);
    voidappendChild(ConfigNode *node);
    boolhasChild(const QString &name) const;
    TreeViewModel   *getChildren();
    TreeViewModel   *getMetaValue();
    boolchildrenHaveNoChildren() const;
    ConfigNode  *getChildByName(QString &name);
    Q_INVOKABLE ConfigNode  *getChildByIndex(int index);

private:
    QString m_name;
    QString m_path;
    QVariant m_value;
    kdb::Key m_key;
    QList<ConfigNode*> m_children;

signals:
    void nameChanged();
};

Q_DECLARE_METATYPE(ConfigNode)

#endif // CONFIGNODE_H
