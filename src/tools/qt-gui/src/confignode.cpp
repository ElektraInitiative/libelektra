#include "confignode.hpp"
#include "treeviewmodel.hpp"

using namespace kdb;

ConfigNode::ConfigNode(const QString& name, const QString& path, const Key &key, TreeViewModel *parentModel)
    : m_name(name)
    , m_path(path)
    , m_key(key)
    , m_children(new TreeViewModel)
    , m_metaData(new TreeViewModel)
    , m_parentModel(parentModel)
    , m_isExpanded(false)
{
    if (m_key && m_key.isString())
        m_value = QVariant::fromValue(QString::fromStdString(m_key.getString()));
    else if (m_key && m_key.isBinary())
        m_value = QVariant::fromValue(QString::fromStdString(m_key.getBinary()));

    if(m_key)
        populateMetaModel();
}

ConfigNode::ConfigNode(const ConfigNode& other)
    : QObject()
    , m_name(other.m_name)
    , m_path(other.m_path)
    , m_value(other.m_value)
    , m_key(other.m_key.dup())
    , m_children(new TreeViewModel())
    , m_metaData(new TreeViewModel())
    , m_parentModel(new TreeViewModel())
    , m_isExpanded(false)
{

    foreach(ConfigNode *node, other.m_children->model())
    {
        m_children->append(new ConfigNode(*node));
    }

    foreach(ConfigNode *node, other.m_metaData->model())
    {
        m_metaData->append(new ConfigNode(*node));
    }
}

ConfigNode::ConfigNode()
    : m_children(new TreeViewModel)
    , m_metaData(new TreeViewModel)
    , m_parentModel(new TreeViewModel)
    , m_isExpanded(false)
{
}

ConfigNode::~ConfigNode()
{
    delete m_children;
    delete m_metaData;
}

int ConfigNode::getChildCount() const
{
    return m_children->model().count();
}

QString ConfigNode::getName() const
{
    return m_name;
}

QString ConfigNode::getPath() const
{
    return m_path;
}

QVariant ConfigNode::getValue() const
{
    return m_value;
}

void ConfigNode::setName(const QString& name)
{
    m_name = name;

    int idx = m_path.lastIndexOf("/");

    if(idx != -1)
    {
        m_path.replace(idx, m_path.length() - idx,"/" + name);
    }

    if(m_key){
        if(QString::fromStdString(m_key.getName()) != ""){
            try{
                m_key.setBaseName(name.toStdString());
            }
            catch(KeyInvalidName ex){
                emit showError(tr("Could not set name because Keyname \"") + QString::fromStdString(m_key.getFullName()) + tr("\" is invalid."), "", ex.what());
            }
        }
    }
}

void ConfigNode::setValue(const QVariant& value)
{
    m_value = value;

    if(m_key)
        m_key.setString(value.toString().toStdString());
}

void ConfigNode::setMeta(const QString &name, const QVariant &value)
{
    m_name = name;
    m_value = value;

    if(m_key)
    {
        m_key.setMeta(name.toStdString(), value.toString().toStdString());
    }
}

void ConfigNode::setMeta(const QVariantMap &metaData)
{
    for(int i = 0; i < m_metaData->model().size(); i++)
    {
        m_metaData->model().at(i)->deleteMeta(m_metaData->model().at(i)->getName());
    }

    m_metaData->clearMetaModel();

    for(int i = 0; i < metaData.size(); i++)
    {
        m_metaData->insertMetaRow(i, this);
    }

    int counter = 0;

    for(QVariantMap::const_iterator iter = metaData.begin(); iter != metaData.end(); iter++)
    {
        QVariantList tmp;
        tmp << iter.key() << iter.value();
        m_metaData->setData(counter, tmp, "MetaValue");
        counter++;
    }
}

void ConfigNode::deleteMeta(const QString &name)
{
    if(m_key)
    {
        //        qDebug() << "ConfigNode::deleteMeta: " << "metakey " << name << " of node " << m_name << " deleted";
        m_key.delMeta(name.toStdString());
    }
}

void ConfigNode::accept(Visitor &visitor)
{
    visitor.visit(this);

    foreach (ConfigNode *node, m_children->model())
        node->accept(visitor);
}

Key ConfigNode::getKey() const
{
    return m_key;
}

void ConfigNode::deletePath(QStringList &path)
{
    if(path.count() == 0)
        return;

    QString name = path.takeFirst();
    int index = getIndexByName(name);
    ConfigNode *node = getChildByName(name);

    node->deletePath(path);

    if(node->getChildCount() == 0)
        m_children->removeRow(index);
}

int ConfigNode::getIndexByName(const QString &name)
{
    for(int i = 0; i < m_children->model().count(); i++)
    {
        if(m_children->model().at(i)->getName() == name)
            return i;
    }

    return -1;
}

TreeViewModel *ConfigNode::getParentModel()
{
    return m_parentModel;
}

void ConfigNode::setParentModel(TreeViewModel *parent)
{
    m_parentModel = parent;
}

void ConfigNode::clear()
{
    foreach(ConfigNode *node, m_children->model()){
        node->clear();
        m_children->removeRow(m_children->getIndexByName(node->getName()));
    }
}
bool ConfigNode::getIsExpanded() const
{
    return m_isExpanded;
}

void ConfigNode::setIsExpanded(bool value)
{
    m_isExpanded = value;
}


void ConfigNode::populateMetaModel()
{
    if (m_key)
    {
        m_key.rewindMeta();
        m_metaData->model().clear();

        while (m_key.nextMeta())
        {
            //            qDebug() << "ConfigNode::populateMetaModel: key " << QString::fromStdString(m_key.getName()) << " has metakey " << QString::fromStdString(m_key.currentMeta().getName());
            ConfigNode* node = new ConfigNode();

            node->setName(QString::fromStdString(m_key.getName()));
            node->setKey(m_key);
            node->setMeta(QString::fromStdString(m_key.currentMeta().getName()), QVariant::fromValue(QString::fromStdString(m_key.currentMeta().getString())));

            m_metaData->model().append(node);
        }
    }
}

void ConfigNode::setKey(Key key)
{
    m_key = key;
}

void ConfigNode::setKeyName(const QString &name)
{
    if(!m_key)
        m_key = Key();

    m_key.setName(name.toStdString());
}

void ConfigNode::appendChild(ConfigNode* node)
{
    node->setParentModel(m_children);
    m_children->append(node);
}

bool ConfigNode::hasChild(const QString& name) const
{
    foreach (ConfigNode * node, m_children->model())
    {
        if (node->getName() == name)
        {
            return true;
        }
    }

    return false;
}

TreeViewModel* ConfigNode::getChildren() const
{
    return m_children;
}

TreeViewModel* ConfigNode::getMetaKeys() const
{
    return m_metaData;
}

ConfigNode* ConfigNode::getChildByName(QString& name) const
{
    foreach (ConfigNode * node, m_children->model())
    {
        if (node->getName() == name)
        {
            return node;
        }
    }

    return NULL;
}

ConfigNode* ConfigNode::getChildByIndex(int index) const
{
    if (index >= 0 && index < m_children->model().length())
        return m_children->model().at(index);

    return NULL;
}

void ConfigNode::setPath(const QString &path)
{
    m_path = path;
    setKeyName(path);

    foreach(ConfigNode *node, m_children->model()){
        node->setPath(m_path + "/" + node->getName());
    }
}

bool ConfigNode::childrenHaveNoChildren() const
{
    int children = 0;

    foreach (ConfigNode * node, m_children->model())
    {
        children += node->getChildCount();
    }

    return children == 0;
}
