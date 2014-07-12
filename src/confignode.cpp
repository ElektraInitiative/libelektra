#include "confignode.hpp"

using namespace kdb;

ConfigNode::ConfigNode()
{

}

ConfigNode::ConfigNode(const QString &name, const QString &path):  m_name(name), m_path(path)
{
}

ConfigNode::ConfigNode(const ConfigNode &other)
{

}

ConfigNode::~ConfigNode()
{

}

//int ConfigNode::rowCount(const QModelIndex &parent) const
//{
//    Q_UNUSED(parent)
//    return m_children.count();
//}

//QVariant ConfigNode::data(const QModelIndex &index, int role) const
//{
//    if (!index.isValid())
//        return QVariant();

//    if (index.row() > (m_children.size()-1) )
//        return QVariant();

//    ConfigNode *node = m_children.at(index.row());

//    switch (role)
//    {

//    case Qt::DisplayRole:

//    case NameRole:
//        return QVariant::fromValue(node->getName());

//    case PathRole:
//        return QVariant::fromValue(node->getPath());

//    case ValueRole:
//        return QVariant::fromValue(node->getValue());

//    case ChildCountRole:
//        return QVariant::fromValue(node->getChildCount());

//    case ChildrenRole:
//        return QVariant::fromValue(node->getChildren());

//    case ChildrenHaveNoChildrenRole:
//        return QVariant::fromValue(node->childrenHaveNoChildren());

//    default:
//        return QVariant();

//    }
//}

int ConfigNode::getChildCount()
{
    return m_children.length();
}

QString ConfigNode::getName()
{
    return m_name;
}

QString ConfigNode::getPath()
{
    return m_path;
}

QString ConfigNode::getValue()
{
    KDB kdb;
    KeySet config;
    kdb.get(config, m_path.toStdString());

    Key k = config.lookup(m_path.toStdString());

    if(k && k.isString())
        return QString::fromStdString(k.getString());
    else if(k && k.isBinary())
        return QString::fromStdString(k.getBinary());
    else
        return "";

}

void ConfigNode::appendChild(ConfigNode *node)
{
    m_children.append(node);
}

bool ConfigNode::hasChild(const QString &name)
{
    foreach(ConfigNode *node, m_children){
        if(node->getName() == name){
            return true;
        }

    }

    return false;
}

TreeViewModel* ConfigNode::getChildren()
{
    return new TreeViewModel(m_children);
}

ConfigNode *ConfigNode::getChildByName(QString &name)
{
    foreach(ConfigNode *node, m_children){
        if(node->getName() == name){
            return node;
        }
    }

    return new ConfigNode("","");
}

ConfigNode *ConfigNode::getChildByIndex(int index)
{
    if(index >= 0 && index < m_children.length())
        return m_children.at(index);
    else
        return new ConfigNode("","");
}

bool ConfigNode::childrenHaveNoChildren()
{
    int children = 0;

    foreach(ConfigNode *node, m_children){
        children += node->getChildCount();
    }

    return children == 0;
}

//QHash<int, QByteArray> ConfigNode::roleNames() const
//{
//    QHash<int, QByteArray> roles;
//    roles[NameRole] = "name";
//    roles[PathRole] = "path";
//    roles[ValueRole] = "value";
//    roles[ChildCountRole] = "childCount";
//    roles[ChildrenRole] = "children";
//    roles[ChildrenHaveNoChildrenRole] = "childrenHaveNoChildren";
//    return roles;
//}


