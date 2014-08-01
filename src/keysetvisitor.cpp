#include "keysetvisitor.hpp"

using namespace kdb;

KeySetVisitor::KeySetVisitor(KeySet &keySet)
    : m_set(keySet)
{

}

void KeySetVisitor::visit(ConfigNode *node)
{
    Key key = node->getKey();

    if(key){
        qDebug() << "Appending key " << QString::fromStdString(key.getName());
        m_set.append(node->getKey());
    }
    else
        qDebug() << "Key of node " << node->getName() << " is null";
}

void KeySetVisitor::visit(TreeViewModel *model)
{
    m_set.clear();
    qDebug() << "===================================";
    foreach (ConfigNode *node, model->model())
        node->accept(*this);
    qDebug() << "===================================";

    model->repopulateModel();
}
