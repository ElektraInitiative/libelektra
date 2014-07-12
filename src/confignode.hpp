#ifndef CONFIGNODE_H
#define CONFIGNODE_H

#include <QObject>
#include <QVariant>
#include <QVariantList>
#include <QtDeclarative/QDeclarativeListProperty>
#include <kdb.hpp>
#include <keyio.hpp>

#include "treeviewmodel.hpp"

class TreeViewModel;

class ConfigNode : public QObject
{
    Q_OBJECT

public:

    ConfigNode();
    explicit ConfigNode(const QString &name, const QString &path);
    ConfigNode(const ConfigNode &other);
    ~ConfigNode();

    int getChildCount();
    QString getName();
    QString getPath();
    QString getValue();
    void appendChild(ConfigNode *node);
    bool hasChild(const QString &name);
    TreeViewModel *getChildren();
    Q_INVOKABLE ConfigNode *getChildByName(QString &name);
    Q_INVOKABLE ConfigNode *getChildByIndex(int index);
    Q_INVOKABLE bool childrenHaveNoChildren();

private:
    QString m_name;
    QString m_path;
    QString m_value;
    QList<ConfigNode*> m_children;
};

Q_DECLARE_METATYPE(ConfigNode)

#endif // CONFIGNODE_H
