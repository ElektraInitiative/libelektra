#include "keysetvisitor.hpp"

using namespace kdb;

KeySetVisitor::KeySetVisitor(KeySet &keySet)
{
    m_set = keySet;
}

void KeySetVisitor::visit(ConfigNode *node)
{
    Key key = node->getKey();

    if(key && key.isValid()){
        qDebug() << "Appending key " << QString::fromStdString(key.getName());
        m_set.append(key);
    }
    else{
        qDebug() << "Key of node " << node->getName() << " is null";
    }
}

void KeySetVisitor::visit(TreeViewModel *model)
{
    qDebug() << "===================================";
    foreach (ConfigNode *node, model->model())
        node->accept(*this);
    qDebug() << "===================================";

    model->populateModel();
}

KeySet &KeySetVisitor::getKeySet()
{
    return m_set;
}
