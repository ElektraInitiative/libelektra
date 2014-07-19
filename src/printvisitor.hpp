#ifndef PRINTVISITOR_HPP
#define PRINTVISITOR_HPP

#include <iostream>
#include "visitor.hpp"

class PrintVisitor : public Visitor
{
public:
    void visit(ConfigNode *node);
};

#endif // PRINTVISITOR_HPP
