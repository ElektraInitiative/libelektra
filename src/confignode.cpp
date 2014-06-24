#include "confignode.h"

using namespace kdb;

ConfigNode::ConfigNode(const QString &name, const QString &path):  m_name(name), m_path(path)
{
}

int ConfigNode::childCount()
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

ConfigNode *ConfigNode::getChildByName(QString &name)
{
    foreach(ConfigNode *node, m_children){
        if(node->getName() == name){
            return node;
        }
    }
}

ConfigNode *ConfigNode::getChildByIndex(int index)
{
    return m_children.at(index);
}

bool ConfigNode::childrenHaveNoChildren()
{
    int children = 0;

    foreach(ConfigNode *node, m_children){
        children += node->childCount();
    }

    return children == 0;
}

QVariantList ConfigNode::getChildren()
{
    QVariantList children;

    foreach(ConfigNode *node, m_children){
        children.append(QVariant::fromValue(node));
    }

    return children;
}
