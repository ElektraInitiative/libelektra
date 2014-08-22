#ifndef TREEVIEWMODEL_H
#define TREEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QDebug>
#include <QtQml>
#include <kdb.hpp>
#include <keyio.hpp>

#include "confignode.hpp"
#include "printvisitor.hpp"
#include "keysetvisitor.hpp"

class ConfigNode;
class Visitor;

class TreeViewModel : public QAbstractListModel
{

    Q_OBJECT

public:

    /**
     * @brief The TreeViewModelRoles enum
     * @
     */
    enum TreeViewModelRoles
    {
        NameRole = Qt::UserRole + 1, ///< The role QML can access the name of a node at a specified index.
        PathRole, ///< The role QML can access the path of a node at a specified index.
        ValueRole, ///< The role QML can access the value of a node at a specified index.
        ChildCountRole, ///< The role QML can access the number of children of a node at a specified index.
        ChildrenRole, ///< The role QML can access the children model of a node at a specified index.
        ChildrenHaveNoChildrenRole, ///< The role QML can access if children of a node at a specified index do have children on their own.
        MetaValueRole, ///< The role QML can access the meta model of a node at a specified index.
        NodeRole ///< The role QML can retrieve the node at a specified index.
    };

    explicit TreeViewModel(QObject* parent =  0);
    explicit TreeViewModel(kdb::KeySet &keySet);

    // Needed for Qt
    TreeViewModel(TreeViewModel const & other);
    ~TreeViewModel();

    // @return the underlying model
    QList<ConfigNode*>& model()
    {
        return m_model;
    }

    //mandatory methods inherited from QAbstractItemModel
    Q_INVOKABLE int             rowCount(const QModelIndex& parent = QModelIndex()) const;
                QVariant        data(const QModelIndex& index, int role = Qt::DisplayRole) const;
                bool            setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
    Q_INVOKABLE bool            insertRow(int row, const QModelIndex &parent = QModelIndex());
    Q_INVOKABLE bool            removeRow(int row, const QModelIndex& parent = QModelIndex());
                Qt::ItemFlags   flags(const QModelIndex& index) const;

    // recursively populate the model
    void                        populateModel();

    void                        accept(Visitor &visitor);

    /**
     * @brief Get the roles of a ConfigNode at the specifies index. Needed to access roles from outside a delegate in QML.
     * @param idx The index of the ConfigNode.
     * @return A map of the roles of the ConfigNode at the specified index.
     */
    Q_INVOKABLE QVariantMap     get(int idx) const;

    /**
      * @brief Find a search term in the model.
      * @param term The search term of interest.
      * @return A model which includes all ConfigNodes that have the search term in their name or value.
      */
    Q_INVOKABLE QVariant        find(const QString& term);
                void            insertMetaRow(int row, ConfigNode *node);
                void            insertRow(int row, ConfigNode* node);
    Q_INVOKABLE void            clear();
    Q_INVOKABLE void            synchronize();
    Q_INVOKABLE void            repopulateModel();
    Q_INVOKABLE void            createNewNode(const QString &path, const QString &value, const QVariantMap metaData);
                void            append(ConfigNode *node);
    Q_INVOKABLE void            setData(int index, const QVariant& value, const QString& role);
                QString         toString();

private:
    void                        sink(ConfigNode* node, QStringList keys, QString path, kdb::Key key);
    void                        find(ConfigNode *node, TreeViewModel *searchResults, const QString term);

    QList<ConfigNode*>          m_model;
    kdb::Key                    m_metaModelParent;
    kdb::KDB                    m_kdb;
    kdb::KeySet                 m_keySet;

protected:
    QHash<int, QByteArray>      roleNames() const;
};

Q_DECLARE_METATYPE(TreeViewModel)

#endif // TREEVIEWMODEL_H
