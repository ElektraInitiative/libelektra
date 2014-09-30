#include "treeviewmodel.hpp"
#include <factory.hpp>
#include <command.hpp>
#include <cmdline.hpp>
#include <external.hpp>
#include <toolexcept.hpp>

using namespace std;
using namespace kdb;

TreeViewModel::TreeViewModel(QObject* parent)
{
    Q_UNUSED(parent);
}

TreeViewModel::TreeViewModel(KeySet &keySet)
    : m_keySet(keySet)
{
    m_kdb.get(m_keySet, "");
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
        return QVariant::fromValue(node->getMetaKeys());

    case NodeRole:
        return QVariant::fromValue(node);

    case ParentModelRole:
        return QVariant::fromValue(node->getParentModel());

    case IndexRole:
        return QVariant::fromValue(index.row());

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
void TreeViewModel::setData(int index, const QVariant& value, const QString& role)
{
    if (index < 0 || index > m_model.size() - 1)
    {
        qDebug() << "TreeViewModel::setData: Wrong index called. model.size = " << m_model.size() << " index = " << index;
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

QString TreeViewModel::toString()
{
    QString model = "\n";

    foreach(ConfigNode *node, m_model){
        model += node->getPath();
        model += "\n";
    }

    return model;
}

void TreeViewModel::deletePath(const QString &path)
{
    QStringList splittedPath = path.split("/");

    QString root = splittedPath.takeFirst();

    if(root == "system")
    {
        m_model.at(0)->deletePath(splittedPath);
    }
    else if(root == "user")
    {
        m_model.at(1)->deletePath(splittedPath);
    }
    else
    {
        qDebug() << "TreeViewModel::deletePath: INVALID_PATH";
    }
}

int TreeViewModel::getIndexByName(const QString &name) const
{
    for(int i = 0; i < m_model.count(); i++){
        if(m_model.at(i)->getName() == name)
            return i;
    }

    return -1;
}

void TreeViewModel::importConfiguration(ConfigNode *node, QString file, QString format, int mergeStrategy)
{

}

void TreeViewModel::exportConfiguration(ConfigNode *node, QString format, QString file)
{

    synchronize();

    file.remove("file://");

    Factory f;

    QByteArray executable = QString("kdb").toLocal8Bit();
    QByteArray commandName = QString("export").toLocal8Bit();
    QByteArray exportName = node->getName().toLocal8Bit();
    QByteArray exportFormat = format.toLocal8Bit();
    QByteArray exportFile = file.toLocal8Bit();

    char *argv[] = {executable.data(), commandName.data(), exportName.data(), exportFormat.data(), exportFile.data(), NULL};

    string command = argv[1];

    try {
        CommandPtr cmd = f.get(command);

        Cmdline cl(5, argv, cmd.get());

        try
        {
            cmd->execute(cl);
        }
        catch (std::invalid_argument const& ia)
        {
            cerr << "Invalid arguments passed: " << ia.what()
                 << endl << endl;
            cerr << cl << endl;
        }
    }
    catch (CommandException const& ce)
    {
        std::cerr << "The command "
                  << command
                  << " terminated unsuccessfully with the info: "
                  << ce.what()
                  << std::endl;
    }
    catch (kdb::Key& key)
    {
        std::cerr << "The command "
                  << command << " failed while accessing the key database"
                  << std::endl;
        printWarnings(cerr, key);
        printError(cerr, key);
    }
    catch (std::exception const& ce)
    {
        std::cerr << "The command "
                  << command
                  << " terminated unsuccessfully with the info: "
                  << std::endl
                  << ce.what()
                  << std::endl;
    }
    catch (...)
    {
        std::cerr << "Unknown error" << std::endl;
    }
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
            newNode = new ConfigNode(name, (path + "/" + name), key, node->getChildren());
        else
            newNode = new ConfigNode(name, (path + "/" + name), NULL, node->getChildren());

        node->appendChild(newNode);

        sink(newNode, keys, node->getPath() + "/" + name, key);
    }
}


void TreeViewModel::populateModel()
{
    ConfigNode* system = new ConfigNode("system", "system", 0, this);
    ConfigNode* user = new ConfigNode("user", "user", 0, this);

    m_model << system << user;

    m_keySet.rewind();

    while (m_keySet.next())
    {
        QString currentKey = QString::fromStdString(m_keySet.current().getName());

        //        qDebug() << "TreeViewModel::populateModel: currentKey: " << currentKey;

        QStringList keys = currentKey.split("/");
        QString root = keys.takeFirst();

        if (root == "system")
        {
            sink(m_model.at(0), keys, "system", m_keySet.current());
        }
        else if (root == "user")
        {
            sink(m_model.at(1), keys, "user", m_keySet.current());
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
        searchResults->model().append(new ConfigNode("NotfoundNode", "There were no results matching your query.", 0, this));
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

    if (row < 0 || row > m_model.size() - 1)
    {
        qDebug() << "Tried to remove row out of bounds. model.size = " <<  m_model.size() << ", index = " << row;
        return false;
    }

    beginRemoveRows(QModelIndex(), row, row);
    m_model.at(row)->invalidateKey();
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
    node->setParentModel(this);

    beginInsertRows(QModelIndex(), row, row);
    m_model.insert(row, node);
    endInsertRows();

    return true;
}

void TreeViewModel::insertRow(int row, ConfigNode *node)
{
    beginInsertRows(QModelIndex(), row, row);
    m_model.insert(row, node);
    endInsertRows();
}


void TreeViewModel::insertMetaRow(int row, ConfigNode *node)
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

    QStringList keys = path.split("/");
    QString root = keys.takeFirst();

    if (root == "system")
    {
        sink(m_model.at(0), keys, "system", key);
    }
    else if (root == "user")
    {
        sink(m_model.at(1), keys, "user", key);
    }
    else
    {
        qDebug() << "TreeViewModel::createNewNode: INVALID_KEY: " << path;
    }
}

void TreeViewModel::append(ConfigNode *node)
{
    insertRow(rowCount(), node);
}

void TreeViewModel::synchronize()
{
    KeySetVisitor ksVisit(m_keySet);
    accept(ksVisit);
    m_keySet = ksVisit.getKeySet();
    m_kdb.set(m_keySet, "/");
    populateModel();
}

void TreeViewModel::clear()
{
    beginResetModel();
    m_model.clear();
    endResetModel();
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
    roles[ParentModelRole] = "parentModel";
    roles[IndexRole] = "index";

    return roles;
}
