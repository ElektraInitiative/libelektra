#include "treeviewmodel.hpp"

using namespace std;
using namespace kdb;

TreeViewModel::TreeViewModel(QObject* parent)
{
    Q_UNUSED(parent)

    /*
    qDebug() << "Name " << NameRole;
    qDebug() << "Path " << PathRole;
    qDebug() << "Value " << ValueRole;
    qDebug() << "ChildCount " << ChildCountRole;
    qDebug() << "Children " << ChildrenRole;
    qDebug() << "CHNC " << ChildrenHaveNoChildrenRole;
    qDebug() << "MetaV " << MetaValueRole;
    qDebug() << "RC " << RowCountRole;
    qDebug() << "NR " << NodeRole;
    */
}

//TreeViewModel::TreeViewModel(QList<ConfigNode*> const & nodes)
//{
//    m_model = nodes; // copy from other list
//}

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

// TODO: why do we have a qml* variant here?
int TreeViewModel::qmlRowCount() const
{
    return rowCount();
}

QVariant TreeViewModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid())
    {
        qDebug() << "index not valid";
        // TODO: why is this function called with wrong index?
        return QVariant();
    }

    if (index.row() > (m_model.size() - 1))
    {
        qDebug() << "row too high" << index.row();
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

    case RowCountRole:
        return QVariant::fromValue(m_model.count());

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
        qDebug() << "Wrong index called";
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
        qDebug() << "setDataValue: Wrong index called. model.size = " << m_model.size() << " index = " << index;
        return;
    }

    QModelIndex modelIndex = this->index(index);

    if (role == "Name")
    {
//        qDebug() << "Name " << value.toString();
        setData(modelIndex, value, NameRole);
    }
    else if (role == "Value")
    {
//        qDebug() << "Value " << value.toString();
        setData(modelIndex, value, ValueRole);
    }
    else if (role == "MetaValue")
    {
//        qDebug() << "MetaValue " << value.toString();
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

// TODO: make recursion more elegant, pass Key
void TreeViewModel::sink(ConfigNode* node, QStringList keys, QString path, Key key)
{
    if (keys.length() == 0)
        return;

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
        // qDebug() << "new child: " << name << " with path: " << (path + "/" + name);
        ConfigNode* newNode = new ConfigNode(name, (path + "/" + name), key);
        node->appendChild(newNode);
        sink(newNode, keys, node->getPath() + "/" + name, key);
    }
}


void TreeViewModel::populateModel(kdb::KeySet const & config)
{
    ConfigNode* system = new ConfigNode("system", "system", 0);
    ConfigNode* user = new ConfigNode("user", "user", 0);

    m_model << system << user;

    QStringList configData;

    config.rewind();
    while (config.next())
    {
        QString currentKey = QString::fromStdString(config.current().getName());

        QStringList splittedKey = currentKey.split("/");

        if (splittedKey.at(0) == "system")
        {
            splittedKey.removeFirst();
            sink(m_model.at(0), splittedKey, "system", config.current());
        }
        else if (splittedKey.at(0) == "user")
        {
            splittedKey.removeFirst();
            sink(m_model.at(1), splittedKey, "user", config.current());
        }
        else
        {
            qDebug() << "INVALID_KEY";
        }

    }

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
    delete m_model.takeAt(row);
    endRemoveRows();

    return true;
}

bool TreeViewModel::insertRow(int row, const QModelIndex& parent)
{
    Q_UNUSED(parent);
    beginInsertRows(QModelIndex(), row, row);
    m_model.insert(row, new ConfigNode());
    endInsertRows();

    return true;
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
    roles[RowCountRole] = "rowCount";
    roles[NodeRole] = "node";
    return roles;
}
