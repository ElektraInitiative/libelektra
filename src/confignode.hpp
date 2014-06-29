#ifndef CONFIGNODE_H
#define CONFIGNODE_H

#include <QObject>
#include <QVariant>
#include <QVariantList>
#include <QtDeclarative/QDeclarativeListProperty>
#include <kdb.hpp>
#include <keyio.hpp>

class ConfigNode : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString name READ getName() NOTIFY nameChanged())
    Q_PROPERTY(QString path READ getPath() NOTIFY pathChanged())
    Q_PROPERTY(QString value READ getValue() NOTIFY valueChanged())
//    Q_PROPERTY(bool childrenHaveNoChildren READ childrenHaveNoChildren() NOTIFY childrenHaveNoChildrenChanged())
    Q_PROPERTY(int childCount READ childCount() NOTIFY childCountChanged())
    Q_PROPERTY(QVariantList children READ getChildren() NOTIFY childrenChanged())

private:
    QString m_name;
    QString m_path;
    QString m_value;
    QList<ConfigNode*> m_children;

public:
    explicit ConfigNode(const QString &name, const QString &path);

    int childCount();
    QString getName();
    QString getPath();
    QString getValue();
    void appendChild(ConfigNode *node);
    bool hasChild(const QString &name);
    QVariantList getChildren();
    Q_INVOKABLE ConfigNode *getChildByName(QString &name);
    Q_INVOKABLE ConfigNode *getChildByIndex(int index);
    Q_INVOKABLE bool childrenHaveNoChildren();

signals:
    void childrenChanged();
    void nameChanged();
    void pathChanged();
    void childCountChanged();
    void valueChanged();
    void childrenHaveNoChildrenChanged();

public slots:

};

#endif // CONFIGNODE_H
