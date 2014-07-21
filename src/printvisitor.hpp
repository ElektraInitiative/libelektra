#ifndef PRINTVISITOR_HPP
#define PRINTVISITOR_HPP

#include <QDebug>
#include "visitor.hpp"
#include "confignode.hpp"

class PrintVisitor : public Visitor
{
public:
    void visit(ConfigNode *node);
    void visit(TreeViewModel *model);
};

#endif // PRINTVISITOR_HPP
