#include "confignode.hpp"
#include "treeviewmodel.hpp"

using namespace kdb;

ConfigNode::ConfigNode(const QString& name, const QString& path, const Key &key)
    : m_name(name)
    , m_path(path)
    , m_key(key)
    , m_children(new TreeViewModel)
    , m_metaData(new TreeViewModel)
{
    // TODO: why not give full path? (or even better, pass Key
    // with getBaseName() and getName())

    if (m_key && m_key.isString())
        m_value = QVariant::fromValue(QString::fromStdString(m_key.getString()));
    else if (m_key && m_key.isBinary())
        m_value = QVariant::fromValue(QString::fromStdString(m_key.getBinary()));

    if(m_key)
        populateMetaModel();
}

ConfigNode::ConfigNode(const ConfigNode& other)
    : QObject()
    , m_children(new TreeViewModel)
    , m_metaData(new TreeViewModel)
{
    m_name = other.m_name;
    m_path = other.m_path;
    m_value = other.m_value;
    m_key = other.m_key;
}

ConfigNode::ConfigNode()
    : m_children(new TreeViewModel)
    , m_metaData(new TreeViewModel)
{
}

ConfigNode::~ConfigNode()
{
    delete m_children;
    delete m_metaData;

    if(m_key)
        m_key.clear();
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
//    qDebug() << "ConfigNode::setName: Node with name " << m_name << " has new name " << name;
    m_name = name;

    if(QString::fromStdString(m_key.getName()) != ""){
//        qDebug() << "ConfigNode::setName: Key with name " << QString::fromStdString(m_key.getName()) << " has new base name " << name;
        m_key.setBaseName(name.toStdString());
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
//    qDebug() << "ConfigNode::setMeta: metaNode " << m_name << " has new metaname " << name;
    m_name = name;
    m_value = value;

    if(m_key){
//        deleteMeta(m_name);
//        qDebug() << "ConfigNode::setMeta: key " << QString::fromStdString(m_key.getName()) << " has new metakey " << name;
        m_key.setMeta(name.toStdString(), value.toString().toStdString());
    }
}

void ConfigNode::deleteMeta(const QString &name)
{
    if(m_key){
        qDebug() << "metakey " << name << " of node " << m_name << " deleted";
        m_key.delMeta(name.toStdString());
    }
}

void ConfigNode::accept(Visitor &visitor)
{
    visitor.visit(this);

    foreach (ConfigNode *node, m_children->model())
        node->accept(visitor);
}

Key ConfigNode::getKey()
{
    return m_key;
}

void ConfigNode::deleteKey()
{
    qDebug() << "clearing key " << QString::fromStdString(m_key.getName());
    m_key.clear();
}

void ConfigNode::populateMetaModel()
{
    if (m_key)
    {
        m_key.rewindMeta();
        m_metaData->model().clear();

        while (m_key.nextMeta())
        {
      //      qDebug() << "ConfigNode::populateMetaModel: key " << QString::fromStdString(m_key.getName()) << " has metakey " << QString::fromStdString(m_key.currentMeta().getName());
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

void ConfigNode::appendChild(ConfigNode* node)
{
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

TreeViewModel* ConfigNode::getChildren()
{
    return m_children;
}

TreeViewModel* ConfigNode::getMetaValue()
{
    return m_metaData;
}

ConfigNode* ConfigNode::getChildByName(QString& name)
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

ConfigNode* ConfigNode::getChildByIndex(int index)
{
    if (index >= 0 && index < m_children->model().length())
        return m_children->model().at(index);

    return NULL;
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
