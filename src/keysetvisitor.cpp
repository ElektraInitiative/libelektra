#include "keysetvisitor.hpp"

using namespace kdb;

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
    m_kdb.get(m_set, "/");

    foreach (ConfigNode *node, model->model())
        node->accept(*this);

    m_kdb.set(m_set, "/");

    model->repopulateModel(m_set);
}
