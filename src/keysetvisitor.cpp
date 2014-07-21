#include "keysetvisitor.hpp"

void KeySetVisitor::visit(ConfigNode *node)
{
    set.append(node->getKey());
}

void KeySetVisitor::visit(TreeViewModel *model)
{
    foreach (ConfigNode *node, model->model())
        node->accept(*this);

    qDebug() << "set size " << set.size();
}
