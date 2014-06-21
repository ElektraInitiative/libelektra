#include "confignode.h"

ConfigNode::ConfigNode(const QString &name, const QString &path):  m_name(name), m_path(path)
{
}

int ConfigNode::childCount()
{
    return m_children.length();
}

void ConfigNode::appendChild(ConfigNode *node)
{
    m_children.append(QVariant::fromValue(node));
}

bool ConfigNode::hasChild(const QString &name)
{
//    foreach(QVariant node, m_children){
//        if(node == name){
//            return true;
//        }

//    }

    return false;
}
