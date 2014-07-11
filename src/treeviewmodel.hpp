#ifndef TREEVIEWMODEL_H
#define TREEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QDebug>
#include <QtQml>
#include <kdb.hpp>
#include <keyio.hpp>

#include "confignode.hpp"

class TreeViewModel : public QAbstractListModel {
    Q_OBJECT

    //    Q_PROPERTY(QVariantList model READ getModel() NOTIFY modelChanged())

public:

    enum TreeViewModelRoles {
        NameRole = Qt::UserRole + 1,
        PathRole,
        ValueRole,
        ChildCountRole,
        ChildrenRole,
        ChildrenHaveNoChildrenRole
    };

    explicit TreeViewModel(QObject *parent =  0);
    TreeViewModel(const TreeViewModel &other);
    ~TreeViewModel();
    //    QVariantList getModel();
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    void sink(ConfigNode *node, QStringList keys, QString path);

    //    Q_INVOKABLE void synchronize();
    //    Q_INVOKABLE void deleteKey(const QString &path);

private:
    QList<ConfigNode*> m_model;
    void populateModel();
    //    QQmlContext *m_ctxt;

    kdb::KeySet m_config;
    kdb::KDB m_kdb;

protected:
    QHash<int, QByteArray> roleNames() const;

signals:
    void modelChanged();

public slots:

};

Q_DECLARE_METATYPE(TreeViewModel)

#endif // TREEVIEWMODEL_H
