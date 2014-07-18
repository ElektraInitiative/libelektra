#include "confignode.hpp"
#include "treeviewmodel.hpp"

using namespace kdb;

ConfigNode::ConfigNode(const QString& name, const QString& path)
    : m_name(name)
    , m_path(path)
    , m_children(new TreeViewModel)
{
    // TODO: why not give full path? (or even better, pass Key
    // with getBaseName() and getName())
    // TODO: avoid rereading the whole database dozens of times!
    // (pass Key here)
    KDB kdb;
    KeySet config;
    kdb.get(config, m_path.toStdString());

    m_key = config.lookup(m_path.toStdString());

    if (m_key && m_key.isString())
        m_value = QVariant::fromValue(QString::fromStdString(m_key.getString()));

    else if (m_key && m_key.isBinary())
        m_value = QVariant::fromValue(QString::fromStdString(m_key.getBinary()));
}

ConfigNode::ConfigNode(const ConfigNode& other)
    : QObject()
    , m_children(new TreeViewModel)
{
    m_name = other.m_name;
    m_path = other.m_path;
    m_value = other.m_value;
    m_key = other.m_key;
}

ConfigNode::ConfigNode()
    : m_children(new TreeViewModel)
{
}

ConfigNode::~ConfigNode()
{
    delete m_children;
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
    // TODO: key is not updated
    emit nameChanged();
}

void ConfigNode::setValue(const QVariant& value)
{
    m_value = value;
    // TODO: key is not updated
    // TODO: no signal emitted
}

void ConfigNode::appendChild(ConfigNode* node)
{
    m_children->model().append(node);
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
    QList<ConfigNode*> meta;

    if (m_key)
    {
        m_key.rewindMeta();

        while (m_key.nextMeta())
        {
            ConfigNode* node = new ConfigNode();
            node->setName(QString::fromStdString(m_key.currentMeta().getName()));
            node->setValue(QString::fromStdString(m_key.currentMeta().getString()));
            meta << node;
        }
    }

    // TODO: dangling reference, will crash on modifications
    // (to be fixed by holding TreeViewModel)
    return new TreeViewModel(meta);
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

    // TODO: dangling reference (return 0?)
    return new ConfigNode("", "");
}

ConfigNode* ConfigNode::getChildByIndex(int index)
{
    if (index >= 0 && index < m_children->model().length())
        return m_children->model().at(index);

    // TODO: dangling reference (return 0?)
    return new ConfigNode("", "");
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
