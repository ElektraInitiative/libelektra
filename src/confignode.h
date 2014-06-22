#ifndef CONFIGNODE_H
#define CONFIGNODE_H

#include <QObject>
#include <QVariant>
#include <QVariantList>
#include <QtDeclarative/QDeclarativeListProperty>

class ConfigNode : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString name READ getName() NOTIFY nameChanged())
    Q_PROPERTY(QString path MEMBER m_path NOTIFY pathChanged())
    Q_PROPERTY(int childCount READ childCount() NOTIFY childCountChanged())
    Q_PROPERTY(QVariantList children READ getChildren() NOTIFY childrenChanged())

private:
    QString m_name;
    QString m_path;
    QList<ConfigNode*> m_children;

public:
    explicit ConfigNode(const QString &name, const QString &path);

    int childCount();
    QString getName();
    QString getPath();
    void appendChild(ConfigNode *node);
    bool hasChild(const QString &name);
    QVariantList getChildren();
    ConfigNode *getChild(QString &name);
signals:
    void childrenChanged();
    void nameChanged();
    void pathChanged();
    void childCountChanged();

public slots:

};

#endif // CONFIGNODE_H
