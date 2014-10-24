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
        NameRole = Qt::UserRole + 1, ///< The role QML can access the name of a ConfigNode at a specified index.
        PathRole, ///< The role QML can access the path of a ConfigNode at a specified index.
        ValueRole, ///< The role QML can access the value of a ConfigNode at a specified index.
        ChildCountRole, ///< The role QML can access the number of children of a ConfigNode at a specified index.
        ChildrenRole, ///< The role QML can access the children model of a ConfigNode at a specified index.
        ChildrenHaveNoChildrenRole, ///< The role QML can access if children of a ConfigNode at a specified index do have children on their own.
        MetaValueRole, ///< The role QML can access the meta model of a ConfigNode at a specified index.
        NodeRole, ///< The role QML can retrieve the ConfigNode at a specified index.
        ParentModelRole, ///< The role QML can retrieve a pointer to the ParentModel of a ConfigNode.
        IndexRole, ///< The role QML can retrieve the index of a ConfigNode.
        IsNullRole,
        IsExpandedRole,
        CountRole
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
    QVariant                    data(const QModelIndex& index, int role = Qt::DisplayRole) const;
    bool                        setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
    Q_INVOKABLE bool            insertRow(int row, const QModelIndex &parent = QModelIndex());
    Q_INVOKABLE bool            removeRow(int row, const QModelIndex& parent = QModelIndex());
    Qt::ItemFlags               flags(const QModelIndex& index) const;

    // recursively populate the model
    Q_INVOKABLE void            populateModel();

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

    /**
     * @brief Inserts a new ConfigNode at a specified index into this TreeViewModel. This method is used if this TreeViewModel is holding meta keys.
     *
     * @param row The index the new ConfigNode is supposed to be inserted at.
     * @param node The ConfigNode that is supposed to be inserted.
     */
    void                        insertMetaRow(int row, ConfigNode *node);

    /**
     * @brief Inserts a new ConfigNode at a specified index into this TreeViewModel. This method is used if this TreeViewModel is holding non metakey ConfigNodes.
     *
     * @param row The index the new ConfigNode is supposed to be inserted at.
     * @param node The ConfigNode that is supposed to be inserted.
     */
    void                        insertRow(int row, ConfigNode* node);

    /**
     * @brief Resets this TreeViewModel to an empty state.
     *
     */
    Q_INVOKABLE void            clear();

    /**
     * @brief Looks for valid ConfigNodes, adds them to a KeySet and repopulates this TreeViewModel based on the KeySet.
     *
     */
    Q_INVOKABLE void            synchronize();

    /**
     * @brief Creates a new ConfigNode and puts it into its place in the hierarchy.
     *
     * @param path The path (full keyname) of the ConfigNode.
     * @param value The value of the ConfigNode.
     * @param metaData The metada of the ConfigNode.
     */
    Q_INVOKABLE void            createNewNode(const QString &path, const QString &value, const QVariantMap metaData);

    /**
     * @brief Appends a ConfigNode to this TreeViewModel. At the time of insertion the index of the ConfigNode will be the largest in this model.
     *
     * @param node The ConfigNode that is appended to this TreeViewModel.
     */
    void                        append(ConfigNode *node);

    /**
     * @brief A version of the mandatory setData method that can be called from QML without a QModelIndex. It creates a QModelIndex and calls the mandatory setData method.
     *
     * @param index The index of the ConfigNode inside this TreeViewModel that is supposed to be manipulated.
     * @param value Holds the data to manipulate the ConfigNode.
     * @param role Holds the role name that determines the type of the manipulation.
     */
    Q_INVOKABLE void            setData(int index, const QVariant& value, const QString& role);

    /**
     * @brief This method exists only for debugging purposes and is very likely to be removed.
     *
     * @return QString A String representation of this model.
     */
    QString                     toString();

    /**
     * @brief This method is needed to support undoing the creation of a new ConfigNode. Since a new ConfigNode is added via the @see #sink method, it
     * is not possible to say where the new ConfigNode will find its place. This method is supposed to "clean up" the path of the new ConfigNode, if the insertion
     * is going to be reverted.
     * @param path The whole path of the ConfigNode which should be removed.
     */
    void                        deletePath(const QString &path);

    /**
     * @brief Returns the index of a ConfigNode in this TreeViewModel based in the ConfigNode's name.
     *
     * @param name The name of the ConfigNode.
     * @return int The index of the ConfigNode, -1 if a ConfigNode with this name is not present in this TreeViewModel.
     */
    Q_INVOKABLE int             getIndexByName(const QString &name) const;

    /**
     * @brief Export the configuration below a ConfigNode to a file on the harddrive.
     *
     * @param node The ConfigNode that is the root node of the exported configuration.
     * @param format Specifies the file format of the exported file.
     * @param file The path on the harddisk where the exported file is written to.
     */
    Q_INVOKABLE void            exportConfiguration(ConfigNode *node, QString format, QString file);

    Q_INVOKABLE void            importConfiguration(const QString &name, const QString &file, QString &format, const QString &mergeStrategy);

    void setKeySet(kdb::KeySet set);

    void collectCurrentKeySet();
    void clearMetaModel();

    Q_INVOKABLE QStringList     getMountedBackends();
    Q_INVOKABLE void            unMountBackend(QString backendName);
    Q_INVOKABLE void            reloadModel();
    Q_INVOKABLE int             count() const;

private:

    /**
     * @brief The method that populates this TreeViewModel.
     *
     * @param node The ConfigNode that is supposed to find its place in the hierarchy.
     * @param keys The path of the ConfigNode that is supposed to find its place in the hierarchy, splitted up into a QStringList.
     * @param path The current path of the ConfigNode.
     * @param key The Key that the ConfigNode holds. If it is no leaf node, the Key is NULL.
     */
    void                        sink(ConfigNode* node, QStringList keys, QString path, kdb::Key key);

    /**
     * @brief A private method that is called from the public @see #find method. It performs the actual search.
     *
     * @param node The ConfigNode that is to be searched next.
     * @param searchResults The TreeViewModel that holds the search results.
     * @param term The term that is searched for.
     */
    void                        find(ConfigNode *node, TreeViewModel *searchResults, const QString term);

    QList<ConfigNode*>          m_model;
    kdb::Key                    m_metaModelParent;
    kdb::KDB                    m_kdb;
    kdb::KeySet                 m_keySet;

protected:
    QHash<int, QByteArray>      roleNames() const;

signals:
    void showError(QString text, QString informativeText, QString detailedText) const;
};

Q_DECLARE_METATYPE(TreeViewModel)

#endif // TREEVIEWMODEL_H
