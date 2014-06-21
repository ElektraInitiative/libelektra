#ifndef CONFIGNODE_H
#define CONFIGNODE_H

#include <QObject>
#include <QVariant>
#include <QVariantList>
#include <QtDeclarative/QDeclarativeListProperty>

class ConfigNode : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString name MEMBER m_name NOTIFY nameChanged())
    Q_PROPERTY(QString path MEMBER m_path NOTIFY pathChanged())
    Q_PROPERTY(int childCount READ childCount() NOTIFY childCountChanged())
    Q_PROPERTY(QVariantList children MEMBER m_children NOTIFY childrenChanged())
//    Q_PROPERTY(QVariantList children READ children() NOTIFY childrenChanged())
//    Q_PROPERTY(QList<QVariant> children READ children() NOTIFY childrenChanged())
//    Q_PROPERTY(QVariant children READ children() NOTIFY childrenChanged())
//    Q_PROPERTY(QList<QObject*> children MEMBER m_children NOTIFY childrenChanged())

private:
    QString m_name;
    QString m_path;
    QVariantList m_children;

public:
    explicit ConfigNode(const QString &name, const QString &path);

    int childCount();
    QString getName();
    QString getPath();
    void appendChild(ConfigNode *node);
    bool hasChild(const QString &name);

signals:
    void childrenChanged();
    void nameChanged();
    void pathChanged();
    void childCountChanged();

public slots:

};

#endif // CONFIGNODE_H
