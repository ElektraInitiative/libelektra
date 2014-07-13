#include "treeviewmodel.hpp"

using namespace std;
using namespace kdb;

TreeViewModel::TreeViewModel(QObject *parent)
{
    Q_UNUSED(parent)
    populateModel();
}

TreeViewModel::TreeViewModel(QList<ConfigNode *> nodes)
{
    m_model = nodes;
}

TreeViewModel::TreeViewModel(const TreeViewModel &other)
{
    Q_UNUSED(other)
}

TreeViewModel::~TreeViewModel()
{
    qDeleteAll(m_model);
}

int TreeViewModel::rowCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return m_model.count();
}

int TreeViewModel::qmlRowCount() const
{
    return m_model.count();
}

QVariant TreeViewModel::data(const QModelIndex &index, int role) const
{
    //    qDebug() << index;

    if (!index.isValid())
        return QVariant();

    if (index.row() > (m_model.size()-1) )
        return QVariant();

    ConfigNode *node = m_model.at(index.row());

    switch (role)
    {

    case Qt::DisplayRole:

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
        return QVariant();

    }
}

bool TreeViewModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
    Q_UNIMPLEMENTED();
}

bool TreeViewModel::insertRows(int row, int count, const QModelIndex &parent)
{
    Q_UNIMPLEMENTED();
}

bool TreeViewModel::removeRow(int row, const QModelIndex &parent)
{
    Q_UNUSED(parent);

    beginRemoveRows(QModelIndex(), row, row);

    delete m_model.takeAt(row);

    endRemoveRows();

    return true;
}

Qt::ItemFlags TreeViewModel::flags(const QModelIndex &index) const
{
    if (!index.isValid())
        return Qt::ItemIsEnabled;

    return QAbstractItemModel::flags(index) | Qt::ItemIsEditable;
}

void TreeViewModel::sink(ConfigNode *node, QStringList keys, QString path){

    if(keys.length() == 0)
        return;

    QString name =  keys.takeFirst();

    if(node->hasChild(name)){
        sink(node->getChildByName(name), keys, node->getPath() + "/" + name);
    }
    else{
        ConfigNode *newNode = new ConfigNode(name, (path + "/" + name));
        node->appendChild(newNode);
        sink(newNode, keys, node->getPath() + "/" + name);
    }
}

void TreeViewModel::populateModel()
{
    m_kdb.get(m_config, "/");
    m_config.rewind();

    ConfigNode *system = new ConfigNode("system", "system");
    ConfigNode *root = new ConfigNode("user", "user");

    m_model << system << root;

    QStringList configData;

    //    qDebug() << "In populateModel: ";

    while(m_config.next()){
        configData << QString::fromStdString(m_config.current().getName());
        //        qDebug() << QString::fromStdString(m_config.current().getName());
    }

    for(int i = 0; i < configData.length(); i++){

        QStringList tmpKeys = configData.at(i).split("/");

        if(tmpKeys.at(0) == "system"){
            tmpKeys.removeFirst();
            sink(m_model.at(0), tmpKeys, "system");
        }
        else if(tmpKeys.at(0) == "user"){
            tmpKeys.removeFirst();
            sink(m_model.at(1), tmpKeys, "user");
        }
        else{
            qDebug() << "INVALID_KEY";
        }

    }

    emit modelChanged();

    qDebug() << "POPULATED===========================================!";
}

QVariantMap TreeViewModel::get(int idx) const {
    QVariantMap map;
    foreach(int k, roleNames().keys()) {
        map[roleNames().value(k)] = data(index(idx, 0), k);
    }
    return map;
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
