#include "printvisitor.hpp"

void PrintVisitor::visit(ConfigNode *node)
{
    qDebug() << node->getName();
}

void PrintVisitor::visit(TreeViewModel *model)
{
    foreach (ConfigNode *node, model->model())
        node->accept(*this);
}
