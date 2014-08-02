#include "treeviewmodel.hpp"

using namespace std;
using namespace kdb;

TreeViewModel::TreeViewModel(QObject* parent)
{
    Q_UNUSED(parent);
    m_kdb.get(m_keySet, "/");
}

TreeViewModel::TreeViewModel(KeySet &keySet)
    : m_keySet(keySet)
{

}

TreeViewModel::TreeViewModel(const TreeViewModel& other)
    : QAbstractListModel()
{
    m_model = other.m_model; // copy from other list
}

TreeViewModel::~TreeViewModel()
{
    // TODO: is this needed?
    qDeleteAll(m_model);
}

int TreeViewModel::rowCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return m_model.count();
}

QVariant TreeViewModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid())
    {
        qDebug() << "TreeViewModel::data: index not valid. Index = " << index.row() << " Model size = " << m_model.size();
        // TODO: why is this function called with wrong index?
        return QVariant();
    }

    if (index.row() > (m_model.size() - 1))
    {
        qDebug() << "TreeViewModel::data: row too high" << index.row();
        // TODO: why is this function called with wrong index?
        return QVariant();
    }

    ConfigNode* node = m_model.at(index.row());

    //qDebug() << "Role: " << role;

    switch (role)
    {

    case Qt::DisplayRole:
        // TODO: document fallthrough if it was desired
    case NameRole:
        return QVariant::fromValue(node->getName());

    case PathRole:
        return QVariant::fromValue(node->getPath());

    case ValueRole:
        return QVariant::fromValue(node->getValue());

    case ChildCountRole:
        return QVariant::fromValue(node->getChildCount());

    case ChildrenRole:
        return QVariant::fromValue(node->getChildren());

    case ChildrenHaveNoChildrenRole:
        return QVariant::fromValue(node->childrenHaveNoChildren());

    case MetaValueRole:
        return QVariant::fromValue(node->getMetaValue());

    case NodeRole:
        return QVariant::fromValue(node);

    default:
        qDebug() << "Unknown role " << role;
        return QVariant();

    }

}

bool TreeViewModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || index.row() > (m_model.size() - 1))
    {
        qDebug() << "TreeViewModel::setData: Wrong index called";
        return false;
    }

    ConfigNode* node = m_model.at(index.row());

    switch (role)
    {

    case NameRole:
        node->setName(value.toString());
        break;

    case ValueRole:
        node->setValue(value);
        break;

    case MetaValueRole:
        QVariantList valueList = value.toList();
        node->setMeta(valueList.at(0).toString(), valueList.at(1));
    }

    emit dataChanged(index, index);

    return true;
}

// TODO: Why are there two implementations of setData needed?
// Because QML cannot call setData() directly (see https://bugreports.qt-project.org/browse/QTBUG-7932)
void TreeViewModel::setDataValue(int index, const QVariant& value, const QString& role)
{
    if (index < 0 || index > m_model.size() - 1)
    {
        qDebug() << "TreeViewModel::setDataValue: Wrong index called. model.size = " << m_model.size() << " index = " << index;
        return;
    }

    QModelIndex modelIndex = this->index(index);

    if (role == "Name")
    {
        setData(modelIndex, value, NameRole);
    }
    else if (role == "Value")
    {
        setData(modelIndex, value, ValueRole);
    }
    else if (role == "MetaValue")
    {
        setData(modelIndex, value, MetaValueRole);
    }
    else
        return;
}

Qt::ItemFlags TreeViewModel::flags(const QModelIndex& index) const
{
    if (!index.isValid())
        return Qt::ItemIsEnabled;

    return QAbstractItemModel::flags(index) | Qt::ItemIsEditable;
}

void TreeViewModel::sink(ConfigNode* node, QStringList keys, QString path, Key key)
{
    if (keys.length() == 0)
        return;

    bool isLeaf = (keys.length() == 1);

    // qDebug() << "in sink: " << keys << " with path: " << path;

    QString name =  keys.takeFirst();
    // qDebug() << "with name: " << name << " and node " << node;

    if (node->hasChild(name))
    {
        // qDebug() << "has child: " << name << " with path: " << node->getPath();
        sink(node->getChildByName(name), keys, node->getPath() + "/" + name, key);
    }
    else
    {
        ConfigNode* newNode;

        if(isLeaf)
            newNode = new ConfigNode(name, (path + "/" + name), key);
        else
            newNode = new ConfigNode(name, (path + "/" + name), NULL);

        node->appendChild(newNode);

        sink(newNode, keys, node->getPath() + "/" + name, key);
    }
}


void TreeViewModel::populateModel()
{
    ConfigNode* system = new ConfigNode("system", "system", 0);
    ConfigNode* user = new ConfigNode("user", "user", 0);

    m_model << system << user;

    m_keySet.rewind();

    while (m_keySet.next())
    {
        QString currentKey = QString::fromStdString(m_keySet.current().getName());

        QStringList splittedKey = currentKey.split("/");

        if (splittedKey.at(0) == "system")
        {
            splittedKey.removeFirst();
            sink(m_model.at(0), splittedKey, "system", m_keySet.current());
        }
        else if (splittedKey.at(0) == "user")
        {
            splittedKey.removeFirst();
            sink(m_model.at(1), splittedKey, "user", m_keySet.current());
        }
        else
        {
            qDebug() << "TreeViewModel::populateModel: INVALID_KEY: " << currentKey;
        }

    }

}

void TreeViewModel::accept(Visitor &visitor)
{
    visitor.visit(this);
}

QVariantMap TreeViewModel::get(int idx) const
{
    QVariantMap map;

    foreach (int k, roleNames().keys())
    {
        map[roleNames().value(k)] = data(index(idx, 0), k);
    }

    return map;
}

QVariant TreeViewModel::find(const QString& term)
{
    TreeViewModel* searchResults = new TreeViewModel;

    foreach (ConfigNode* node, m_model)
    {
        find(node, searchResults, term);
    }

    if (searchResults->model().count() == 0)
    {
        searchResults->model().append(new ConfigNode("NotfoundNode", "There were no results matching your query.", 0));
    }

    return QVariant::fromValue(searchResults);
}

void TreeViewModel::find(ConfigNode* node, TreeViewModel *searchResults, const QString term)
{

    int tmpChildCount = node->getChildCount();

    if (tmpChildCount > 0)
    {
        for (int i = 0; i < tmpChildCount; i++)
        {
            find(node->getChildByIndex(i), searchResults, term);
        }
    }

    if (node->getName().contains(term) || node->getValue().toString().contains(term))
    {
        searchResults->model().append(node);
    }

}

bool TreeViewModel::removeRow(int row, const QModelIndex& parent)
{
    Q_UNUSED(parent);

    if (row < 0 || row > m_model.size()-1)
    {
        qDebug() << "Tried to remove row out of bounds. model.size = " <<  m_model.size() << ", index = " << row;
        return false;
    }

    beginRemoveRows(QModelIndex(), row, row);
    m_model.at(row)->deleteKey();
    delete m_model.takeAt(row);
    endRemoveRows();

    return true;
}

bool TreeViewModel::insertRow(int row, const QModelIndex& parent)
{
    Q_UNUSED(parent);
    ConfigNode *node = new ConfigNode;
    node->setName(QString::fromStdString(m_metaModelParent.getName()));
    node->setKey(m_metaModelParent);
    beginInsertRows(QModelIndex(), row, row);
    m_model.insert(row, node);
    endInsertRows();

    return true;
}


void TreeViewModel::qmlInsertRow(int row, ConfigNode *node)
{
    m_metaModelParent = node->getKey();

    if(m_metaModelParent){
        insertRow(row);
    }
    else
        qDebug() << "Key " << QString::fromStdString(node->getKey().getFullName()) << " not valid!";
}

void TreeViewModel::createNewNode(const QString &path, const QString &value, const QVariantMap metaData)
{
    qDebug() << "TreeViewModel::createNewNode: path = " << path << " value = " << value;
    Key key;
    key.setName(path.toStdString());
    key.setString(value.toStdString());

    for(QVariantMap::const_iterator iter = metaData.begin(); iter != metaData.end(); iter++)
    {
        qDebug() << iter.key() << iter.value();
        key.setMeta(iter.key().toStdString(), iter.value().toString().toStdString());
    }

    QStringList splittedKey = path.split("/");

    if (splittedKey.at(0) == "system")
    {
        splittedKey.removeFirst();
        sink(m_model.at(0), splittedKey, "system", key);
    }
    else if (splittedKey.at(0) == "user")
    {
        splittedKey.removeFirst();
        sink(m_model.at(1), splittedKey, "user", key);
    }
    else
    {
        qDebug() << "TreeViewModel::createNewNode: INVALID_KEY: " << path;
    }
}

void TreeViewModel::append(ConfigNode *node)
{
    beginInsertRows(QModelIndex(), m_model.size(), m_model.size());
    m_model.append(node);
    endInsertRows();
}

void TreeViewModel::synchronize()
{
    KeySetVisitor ksVisit(m_keySet);
    accept(ksVisit);
    m_kdb.set(ksVisit.getKeySet(), "/");
//    qDebug() << "Last Key is " << QString::fromStdString(ksVisit.getKeySet().tail().getFullName());
}

void TreeViewModel::clear()
{
    beginResetModel();
    m_model.clear();
    endResetModel();
}

void TreeViewModel::repopulateModel()
{
//    beginResetModel();
    m_model.clear();
    populateModel();
    //    endResetModel();
}

QHash<int, QByteArray> TreeViewModel::roleNames() const
{
    QHash<int, QByteArray> roles;
    roles[NameRole] = "name";
    roles[PathRole] = "path";
    roles[ValueRole] = "value";
    roles[ChildCountRole] = "childCount";
    roles[ChildrenRole] = "children";
    roles[ChildrenHaveNoChildrenRole] = "childrenHaveNoChildren";
    roles[MetaValueRole] = "metaValue";
    roles[NodeRole] = "node";
    return roles;
}
