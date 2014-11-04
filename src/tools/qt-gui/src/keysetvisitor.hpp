#ifndef KEYSETVISITOR_HPP
#define KEYSETVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"
#include <kdb.hpp>

class KeySetVisitor : public Visitor
{
public:
    explicit KeySetVisitor();
    void visit(ConfigNode *node);
    void visit(TreeViewModel *model);
    kdb::KeySet getKeySet();


private:
    kdb::KeySet m_set;
};

#endif // KEYSETVISITOR_HPP
