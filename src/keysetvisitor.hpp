#ifndef KEYSETVISITOR_HPP
#define KEYSETVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"
#include <kdb.hpp>

class KeySetVisitor : public Visitor
{
public:
    void visit(ConfigNode *node);
    void visit(TreeViewModel *model);

private:
    kdb::KeySet set;
};

#endif // KEYSETVISITOR_HPP
