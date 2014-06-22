#include "confignode.h"

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

ConfigNode *ConfigNode::getChild(QString &name)
{
    foreach(ConfigNode *node, m_children){
        if(node->getName() == name){
            return node;
        }
    }
}

QVariantList ConfigNode::getChildren()
{
    QVariantList children;

    foreach(ConfigNode *node, m_children){
        children.append(QVariant::fromValue(node));
    }

    return children;
}
