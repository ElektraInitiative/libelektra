#ifndef TREEVIEWMODEL_H
#define TREEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QDebug>
#include <QtQml>
#include <kdb.hpp>
#include <keyio.hpp>

#include "confignode.hpp"

class ConfigNode;

class TreeViewModel : public QAbstractListModel {

    Q_OBJECT

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
    TreeViewModel(QList<ConfigNode*> &nodes);
    TreeViewModel(const TreeViewModel &other);
    ~TreeViewModel();

    //mandatory methods
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole);
    Qt::ItemFlags flags(const QModelIndex &index) const;

    void sink(ConfigNode *node, QStringList keys, QString path);

private:
    QList<ConfigNode*> m_model;
    void populateModel();
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
