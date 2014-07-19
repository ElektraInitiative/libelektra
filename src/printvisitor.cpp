#include "printvisitor.hpp"

void PrintVisitor::visit(ConfigNode *node)
{
   qDebug() << node->getName();
}
