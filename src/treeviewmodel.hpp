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
        ChildrenHaveNoChildrenRole,
        MetaValueRole,
        RowCountRole,
        NodeRole
    };

    explicit TreeViewModel(QObject *parent =  0);
    TreeViewModel(QList<ConfigNode *> nodes);
    TreeViewModel(const TreeViewModel &other);
    ~TreeViewModel();

    //mandatory methods inherited from QAbstractItemModel
    int                 rowCount(const QModelIndex &parent = QModelIndex()) const;
    Q_INVOKABLE int     qmlRowCount() const;
    QVariant            data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    bool                setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole);
    Q_INVOKABLE         void setDataValue(int index, const QVariant &value, const QString &role);
    bool                insertRows(int row, int count, const QModelIndex &parent = QModelIndex());
    Q_INVOKABLE bool    removeRow(int row, const QModelIndex &parent = QModelIndex());
    Qt::ItemFlags       flags(const QModelIndex &index) const;

    //recursive populating
    void                sink(ConfigNode *node, QStringList keys, QString path);

    Q_INVOKABLE QVariantMap get(int idx) const;

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
