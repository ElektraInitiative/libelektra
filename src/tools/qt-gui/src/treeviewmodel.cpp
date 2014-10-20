#include "treeviewmodel.hpp"
#include <factory.hpp>
#include <command.hpp>
#include <cmdline.hpp>
#include <external.hpp>
#include <toolexcept.hpp>
#include <backends.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

TreeViewModel::TreeViewModel(QObject* parent)
{
    Q_UNUSED(parent);

    try{
        m_kdb.get(m_keySet, "/");
    }
    catch (kdb::KDBException const & e){
        emit showError(tr("Could not read configuration."), "", QString(e.what()));
    }
}

TreeViewModel::TreeViewModel(KeySet &keySet)
    : m_keySet(keySet)
{
    populateModel();
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
        emit showError(tr("Index not valid."), QString("Index = " + index.row()) + QString("\nModel size = " + m_model.size()), "TreeViewModel::data");
        return QVariant();
    }

    if (index.row() > (m_model.size() - 1))
    {
        emit showError(QString(tr("Index too high. ")), "Index: " + index.row(), "TreeViewModel::data");
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

    case IsNullRole:{
        if(node->getKey())
            return QVariant::fromValue(false);
        else
            return QVariant::fromValue(true);
    }

    default:
        emit showError(tr("Unknown role: ") + role, "", "TreeViewModel::data");
        return QVariant();

    }
}

bool TreeViewModel::setData(const QModelIndex& index, const QVariant& data, int role)
{
    if (!index.isValid() || index.row() > (m_model.size() - 1))
    {
        emit showError(tr("Index not valid."), QString("Index = " + index.row()) + QString("\nModel size = " + m_model.size()),"TreeViewModel::setData");
        return false;
    }

    ConfigNode* node = m_model.at(index.row());

    switch (role)
    {

    case NameRole:
        node->setName(data.toString());
        break;

    case ValueRole:
        node->setValue(data);
        break;

    case MetaValueRole:
        QVariantList valueList = data.toList();
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
        emit showError(tr("Index not valid."), QString("Index = " + index) + QString("\nModel size = " + m_model.size()), "TreeViewModel::setData");
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
        emit showError(tr("Invalid path."), "", "TreeViewModel::deletePath");
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

void TreeViewModel::importConfiguration(const QString &name, const QString &format, QString &file, const QString &mergeStrategy)
{
    collectCurrentKeySet();

    m_keySet.rewind();
    while(m_keySet.next())
        qDebug() << QString::fromStdString(m_keySet.current().getName());

    try{
        m_kdb.set(m_keySet, "/");
    }
    catch (kdb::KDBException const & e){
        emit showError(tr("Importing the configuration from file failed because the current configuration could not be set.") , "", QString(e.what()));
        return;
    }

    file.remove("file://");

    Factory f;

    QByteArray executable = QString("kdb").toLocal8Bit();
    QByteArray commandName = QString("import").toLocal8Bit();
    QByteArray importName = name.toLocal8Bit();
    QByteArray importFormat = format.toLocal8Bit();
    QByteArray importFile = file.toLocal8Bit();
    QByteArray importMergeStrategy = QString("-s" + mergeStrategy).toLocal8Bit();

    char *argv[] = {executable.data(), commandName.data(), importName.data(), importFormat.data(), importFile.data(), importMergeStrategy.data()};
    int argc = sizeof(argv) / sizeof(char*) - 1;

    string command = argv[1];

    try
    {
        CommandPtr cmd = f.get(command);

        Cmdline cl(argc, argv, cmd.get());

        try
        {
            cmd->execute(cl);
        }
        catch (std::invalid_argument const& ia)
        {
            emit showError(tr("Importing the configuration from file failed because there were invalid arguments passed."), "" , QString(ia.what()));
        }
    }
    catch (CommandException const& ce)
    {
        emit showError(tr("Importing the configuration from file terminated unsuccessfully because of a faulty command."), "", QString(ce.what()));
    }
    catch (kdb::Key& key)
    {
        stringstream ws;
        stringstream es;

        ws << printWarnings(cerr, key);
        es << printError(cerr, key);

        emit showError(tr("Importing the configuration from file failed while accessing the key database."), "" , QString::fromStdString(ws.str()) + QString::fromStdString(es.str()));
    }
    catch (std::exception const& ce)
    {
        emit showError(tr("Importing the configuration from file terminated unsuccessfully."), "", QString(ce.what()));
    }
    catch (...)
    {
        emit showError(tr("Unknown error"), "", "TreeViewModel::importConfiguration");
    }

    try{
        m_kdb.get(m_keySet, "");
    }
    catch (kdb::KDBException const & e){
        emit showError(tr("Import: Could not read configuration."), "", QString(e.what()));
    }

    populateModel();
}

void TreeViewModel::exportConfiguration(ConfigNode *node, QString format, QString file)
{
    collectCurrentKeySet();

    try{
        m_kdb.set(m_keySet, "/");
    }
    catch (kdb::KDBException const & e){
        emit showError(tr("Exporting the configuration to file failed because the current configuration could not be set."), "", QString(e.what()));
        return;
    }

    file.remove("file://");

    Factory f;

    QByteArray executable = QString("kdb").toLocal8Bit();
    QByteArray commandName = QString("export").toLocal8Bit();
    QByteArray exportName = node->getPath().toLocal8Bit();
    QByteArray exportFormat = format.toLocal8Bit();
    QByteArray exportFile = file.toLocal8Bit();

    char *argv[] = {executable.data(), commandName.data(), exportName.data(), exportFormat.data(), exportFile.data(), NULL};
    int argc = sizeof(argv) / sizeof(char*) - 1;

    string command = argv[1];

    try {
        CommandPtr cmd = f.get(command);

        Cmdline cl(argc, argv, cmd.get());

        try
        {
            cmd->execute(cl);
        }
        catch (std::invalid_argument const& ia)
        {
            emit showError(tr("Exporting the configuration to file failed because there were invalid arguments passed."), "", QString(ia.what()));
            return;
        }
    }
    catch (CommandException const& ce)
    {
        emit showError(tr("Exporting the configuration to file terminated unsuccessfully."), "", QString(ce.what()));
        return;
    }
    catch (kdb::Key& key)
    {
        stringstream ws;
        stringstream es;

        ws << printWarnings(cerr, key);
        es << printError(cerr, key);

        emit showError(tr("Exporting the configuration to file failed while accessing the key database."), "", QString::fromStdString(ws.str()) + QString::fromStdString(es.str()));
        return;
    }
    catch (std::exception const& ce)
    {
        emit showError(tr("Exporting the configuration to file terminated unsuccessfully."), "", QString(ce.what()));
        return;
    }
    catch (...)
    {
        emit showError(tr("Unknown error"), "", "TreeViewModel::exportConfiguration");
    }
}

void TreeViewModel::setKeySet(KeySet set)
{
    m_keySet.clear();
    set.rewind();

    while(set.next())
        m_keySet.append(Key(set.current().dup()));
}

void TreeViewModel::collectCurrentKeySet()
{
    KeySetVisitor ksVisit;
    accept(ksVisit);

    setKeySet(ksVisit.getKeySet());
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
            newNode = new ConfigNode(name, (path + "/" + name), key.dup(), node->getChildren());
        else
            newNode = new ConfigNode(name, (path + "/" + name), NULL, node->getChildren());

        node->appendChild(newNode);

        sink(newNode, keys, node->getPath() + "/" + name, key);
    }
}

void TreeViewModel::populateModel()
{
    ConfigNode* system = new ConfigNode("system", "system", 0, this);
    ConfigNode* user = new ConfigNode("user", "user", Key("user", KEY_END), this);

    clear();
    //Why wont the treeview update anymore if I clear the List?
    //    m_model.clear();
    m_model << system << user;

    m_keySet.rewind();

    while (m_keySet.next())
    {
        QString currentKey = QString::fromStdString(m_keySet.current().getName());
        //        qDebug() << "current: " << currentKey;
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
        searchResults->model().append(new ConfigNode("NotfoundNode", tr("There were no results matching your query."), 0, this));
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
        emit showError(tr("Index not valid."), QString("Model size = " +  m_model.size()) + QString("\nIndex = " + row), "TreeViewModel::removeRow");
        return false;
    }

    beginRemoveRows(QModelIndex(), row, row);

    if(!m_model.isEmpty())
        m_model.takeAt(row);

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
    node->setParentModel(this);
    m_model.insert(row, node);
    endInsertRows();
}

void TreeViewModel::insertMetaRow(int row, ConfigNode *node)
{
    m_metaModelParent = node->getKey();

    if(m_metaModelParent){
        insertRow(row);
    }
    else{
        QString keyName;

        if(node->getKey())
            keyName = QString::fromStdString(node->getKey().getFullName());
        else
            keyName = node->getName();

        emit showError(tr("Inserting metakey failed."), "Key " + keyName + " is not valid.", "");
    }
}

void TreeViewModel::createNewNode(const QString &path, const QString &value, const QVariantMap metaData)
{
    //    qDebug() << "TreeViewModel::createNewNode: path = " << path << " value = " << value;
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
        emit showError(QString(tr("Creating a new node failed because the key ") + path) + " is invalid", "", "TreeViewModel::createNewNode");
    }
}

void TreeViewModel::append(ConfigNode *node)
{
    insertRow(rowCount(), node);
}

void TreeViewModel::synchronize()
{
    collectCurrentKeySet();

    try{
        m_kdb.set(m_keySet, "/");
    }
    catch (kdb::KDBException const & e){
        emit showError(tr("Synchronizing failed."), "", QString(e.what()));
    }
}

void TreeViewModel::clear()
{
    foreach(ConfigNode *node, m_model){
        node->clear();
    }
}

void TreeViewModel::clearMetaModel()
{
    beginResetModel();
    m_model.clear();
    endResetModel();
}

QStringList TreeViewModel::getMountedBackends()
{
    Backends::BackendInfoVector mtab = Backends::getBackendInfo(m_keySet);

    QStringList mountedBackends;

    for (Backends::BackendInfoVector::const_iterator it = mtab.begin(); it != mtab.end(); ++it)
    {
        mountedBackends.append(QString::fromStdString(it->name));
    }

    return mountedBackends;
}

void TreeViewModel::unMountBackend(QString backendName)
{
    const std::string keyName = string(Backends::mountpointsPath) + "/"  + backendName.toStdString();

    Key x(keyName, KEY_END);

    m_keySet.cut(x);

    populateModel();
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
    roles[IsNullRole] = "isNull";

    return roles;
}
